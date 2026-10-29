"""
Synthetic data generator for the Bramble+ dashboard.

Produces monthly subscription metrics (November 2025 to October 2026)
and supporting tables as CSV files in this folder.

Run: python synthetic_data.py
"""
import numpy as np
import pandas as pd

RNG = np.random.default_rng(2026)
OUT = '.'

MONTHS = ['2025-11', '2025-12', '2026-01', '2026-02', '2026-03', '2026-04',
          '2026-05', '2026-06', '2026-07', '2026-08', '2026-09', '2026-10']
PLANS = ['monthly', 'annual']
CHANNELS = ['organic', 'paid_search', 'referral', 'influencer']
REGIONS = ['DACH', 'Nordics', 'Benelux', 'Southern Europe']
PRICE = {'monthly': 12.99, 'annual': 129 / 12}

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
START_SUBS = 1_790_000
START_MONTHLY_SHARE = 0.655
BASE_CHANNEL_SHARE = np.array([0.50, 0.32, 0.18, 0.0])
NEW_CHANNEL_SHARE = np.array([0.50, 0.32, 0.18])
NEW_ANNUAL_SHARE = 0.22

REGION_SHARE = {                      # by channel
    'organic':     [0.40, 0.22, 0.18, 0.20],
    'paid_search': [0.38, 0.22, 0.20, 0.20],
    'referral':    [0.44, 0.20, 0.18, 0.18],
    'influencer':  [0.62, 0.12, 0.12, 0.14],
}

# monthly cancellation rate
CHURN_RATE = np.array([0.041, 0.044, 0.043, 0.041, 0.040, 0.041,
                           0.039, 0.039, 0.044, 0.045, 0.046, 0.046])
STATUS_CHANGE_MONTH = 6
PAUSE_SHARE = 0.085

BASE_ADDS = np.array([63.8, 66.3, 76.3, 64.3, 63.3, 62.3, 60.3, 79.0, 65.7, 69.7, 68.7, 66.7]) * 1000
PARTNER_ADDS = {4: 74_000, 5: 63_000, 6: 41_000}
INTRO_MONTHS = 3

# relative cancellation propensities
CH_REL = np.array([1.0, 1.12, 0.82])
PLAN_REL = {'monthly': 1.0, 'annual': 0.29}
NEW_SUB_REL = 1.2                     # first two months
PARTNER_HAZARD = [0.025, 0.025, 0.025, 0.24, 0.145, 0.095]
PARTNER_LATER = 0.075

TENURE_START_MEAN = 12.8
MAXAGE = 90

# ---------------------------------------------------------------------------
# Starting base
# ---------------------------------------------------------------------------
ages = np.arange(1, MAXAGE + 1)
q = 1 - 1 / TENURE_START_MEAN
w = q ** (ages - 1)
w /= w.sum()
for _ in range(50):                   # match the starting mean tenure
    m = (w * ages).sum()
    q = q + (TENURE_START_MEAN - m) * 0.002
    w = q ** (ages - 1)
    w /= w.sum()

base = np.zeros((2, 3, MAXAGE))       # plan x channel x age
for ci in range(3):
    base[0, ci] = START_SUBS * BASE_CHANNEL_SHARE[ci] * START_MONTHLY_SHARE * w
    base[1, ci] = START_SUBS * BASE_CHANNEL_SHARE[ci] * (1 - START_MONTHLY_SHARE) * w

cohort = np.zeros((2, 4, 12))         # plan x channel x signup month
paused = np.zeros((2, 4))
records = []


def tenure_now(base, cohort, t):
    b = (base.sum(axis=(0, 1)) * ages).sum()
    c = (cohort.sum(axis=(0, 1)) * np.clip(t - np.arange(12), 0, None)).sum()
    return (b + c) / (base.sum() + cohort.sum())


def paying_mrr(base, cohort, t):
    mrr = 0.0
    for pi, pl in enumerate(PLANS):
        mrr += base[pi].sum() * PRICE[pl]
        for k in range(12):
            for ci in range(4):
                n = cohort[pi, ci, k]
                if n <= 0:
                    continue
                price = PRICE[pl]
                if ci == 3 and k in PARTNER_ADDS and 0 <= t - k < INTRO_MONTHS:
                    price = PRICE[pl] * 0.5
                mrr += n * price
    return mrr


start_tenure = tenure_now(base, cohort, -1)
start_mrr = paying_mrr(base, cohort, -1)

for t in range(12):
    start_active = np.zeros((2, 4))
    start_active[:, :3] = base.sum(axis=2)
    start_active += cohort.sum(axis=2)
    start_total = start_active.sum() + paused.sum()
    start_by = start_active + paused

    target_cancels = CHURN_RATE[t] * start_total
    status_adj = 1.0 / (1 - PAUSE_SHARE) if t >= STATUS_CHANGE_MONTH else 1.0

    # partner cohort exits
    exits = np.zeros((2, 4))
    for k in PARTNER_ADDS:
        if k >= t:
            continue
        a = t - k
        h = PARTNER_HAZARD[a - 1] if a <= len(PARTNER_HAZARD) else PARTNER_LATER
        e = cohort[0, 3, k] * h
        cohort[0, 3, k] -= e
        exits[0, 3] += e
    partner_cancels = exits[0, 3] / status_adj

    # remaining subscribers, scaled to the monthly total
    rel = np.zeros((2, 3, MAXAGE))
    relc = np.zeros((2, 3, 12))
    for pi, pl in enumerate(PLANS):
        for ci in range(3):
            rel[pi, ci] = base[pi, ci] * CH_REL[ci] * PLAN_REL[pl]
            for k in range(t):
                boost = NEW_SUB_REL if (pl == 'monthly' and t - k <= 2) else 1.0
                relc[pi, ci, k] = cohort[pi, ci, k] * CH_REL[ci] * PLAN_REL[pl] * boost
    needed = (target_cancels - partner_cancels) * status_adj
    scale = needed / (rel.sum() + relc.sum())
    e_base = rel * scale
    e_coh = relc * scale
    base -= e_base
    cohort[:, :3, :] -= e_coh
    exits[:, :3] += e_base.sum(axis=2) + e_coh.sum(axis=2)

    if t >= STATUS_CHANGE_MONTH:
        pz = exits * PAUSE_SHARE
        paused += pz
        cancels = exits - pz
    else:
        pz = np.zeros((2, 4))
        cancels = exits

    # age the base
    nb = np.zeros_like(base)
    nb[:, :, 1:] = base[:, :, :-1]
    nb[:, :, -1] += base[:, :, -1]
    base = nb

    # new subscribers
    adds = BASE_ADDS[t] * RNG.normal(1, 0.01)
    for ci in range(3):
        cohort[1, ci, t] = adds * NEW_CHANNEL_SHARE[ci] * NEW_ANNUAL_SHARE
        cohort[0, ci, t] = adds * NEW_CHANNEL_SHARE[ci] * (1 - NEW_ANNUAL_SHARE)
    if t in PARTNER_ADDS:
        cohort[0, 3, t] = PARTNER_ADDS[t]

    end_active = np.zeros((2, 4))
    end_active[:, :3] = base.sum(axis=2)
    end_active += cohort.sum(axis=2)
    records.append(dict(t=t, start_by=start_by, start_total=start_total, exits=exits,
                        cancels=cancels, pauses=pz, adds=cohort[:, :, t].copy(),
                        end_active=end_active, paused=paused.copy(),
                        tenure=tenure_now(base, cohort, t), mrr=paying_mrr(base, cohort, t),
                        cohort=cohort.copy()))

# ---------------------------------------------------------------------------
# Monthly KPIs
# ---------------------------------------------------------------------------
kpi = [dict(month='2025-10', subscribers_end=round(START_SUBS), paying_subscribers_end=round(START_SUBS),
            paused_accounts_end=0, new_subscribers=None, cancellations=None, churn_rate=None,
            mrr_eur=round(start_mrr), arpu_eur=round(start_mrr / START_SUBS, 2),
            marketing_spend_eur=None, cac_eur=None, avg_tenure_months=round(start_tenure, 1))]
CAC_BASE = np.array([39.5, 40.2, 37.8, 38.9, 33.1, 33.8, 35.0, 39.0, 39.8, 39.1, 38.4, 38.1])
for r in records:
    t = r['t']
    subs = r['end_active'].sum() + r['paused'].sum()
    paying = r['end_active'].sum()
    adds = r['adds'].sum()
    spend = adds * CAC_BASE[t] * RNG.normal(1, 0.005)
    kpi.append(dict(month=MONTHS[t], subscribers_end=round(subs), paying_subscribers_end=round(paying),
                    paused_accounts_end=round(r['paused'].sum()), new_subscribers=round(adds),
                    cancellations=round(r['cancels'].sum()),
                    churn_rate=round(r['cancels'].sum() / r['start_total'], 4),
                    mrr_eur=round(r['mrr']), arpu_eur=round(r['mrr'] / paying, 2),
                    marketing_spend_eur=round(spend), cac_eur=round(spend / adds, 2),
                    avg_tenure_months=round(r['tenure'], 1)))
pd.DataFrame(kpi).to_csv(f'{OUT}/bramble_monthly_kpis.csv', index=False)

# ---------------------------------------------------------------------------
# Churn by segment
# ---------------------------------------------------------------------------
rows_ch, rows_pl, rows_rg = [], [], []
for r in records:
    t = r['t']
    sb, rp = r['start_by'], r['cancels']
    for ci, c in enumerate(CHANNELS):
        s, x = sb[:, ci].sum(), rp[:, ci].sum()
        rows_ch.append(dict(month=MONTHS[t], channel=c, subscribers_start=round(s),
                            cancellations=round(x), churn_rate=round(x / s, 4) if s > 0 else None))
    for pi, pl in enumerate(PLANS):
        s, x = sb[pi].sum(), rp[pi].sum()
        rows_pl.append(dict(month=MONTHS[t], plan=pl, subscribers_start=round(s),
                            cancellations=round(x), churn_rate=round(x / s, 4)))
    for ri, rg in enumerate(REGIONS):
        s = sum(sb[:, ci].sum() * REGION_SHARE[c][ri] for ci, c in enumerate(CHANNELS))
        x = sum(rp[:, ci].sum() * REGION_SHARE[c][ri] for ci, c in enumerate(CHANNELS))
        rows_rg.append(dict(month=MONTHS[t], region=rg, subscribers_start=round(s),
                            cancellations=round(x), churn_rate=round(x / s, 4)))
pd.DataFrame(rows_ch).to_csv(f'{OUT}/bramble_churn_by_channel.csv', index=False)
pd.DataFrame(rows_pl).to_csv(f'{OUT}/bramble_churn_by_plan.csv', index=False)
pd.DataFrame(rows_rg).to_csv(f'{OUT}/bramble_churn_by_region.csv', index=False)

# ---------------------------------------------------------------------------
# Cohort retention
# ---------------------------------------------------------------------------
rows_co = []
for k in range(11):
    for ci, c in enumerate(CHANNELS):
        n0 = records[k]['adds'][:, ci].sum()
        if n0 <= 0:
            continue
        for t in range(k, 12):
            n = records[t]['cohort'][:, ci, k].sum()
            rows_co.append(dict(signup_month=MONTHS[k], channel=c, months_since_signup=t - k,
                                signups=round(n0), active=round(n), retained_share=round(n / n0, 4)))
pd.DataFrame(rows_co).to_csv(f'{OUT}/bramble_cohort_retention.csv', index=False)

# ---------------------------------------------------------------------------
# Pricing test
# ---------------------------------------------------------------------------
pd.DataFrame([
    dict(arm='Test', price_eur=9.99, start_date='2025-12-15', end_date='2025-12-31',
         enrollment='Email offer, opt in', subscribers=6200, monthly_plan=1550, annual_plan=4650,
         cancellations_30_days=180, churn_rate_30_days=round(180 / 6200, 4)),
    dict(arm='Control', price_eur=12.99, start_date='2025-12-15', end_date='2025-12-31',
         enrollment='All other subscribers, sample', subscribers=24000, monthly_plan=15360,
         annual_plan=8640, cancellations_30_days=1056, churn_rate_30_days=round(1056 / 24000, 4)),
]).to_csv(f'{OUT}/bramble_pricing_test.csv', index=False)

# ---------------------------------------------------------------------------
# Exit survey (July to October 2026)
# ---------------------------------------------------------------------------
canc = int(round(sum(r['cancels'].sum() for r in records[8:12])))
resp = int(round(canc * 0.06))
reasons = [('Price', 0.41), ('Not enough new content', 0.19), ('Taking a break', 0.12),
           ('Switching to another service', 0.11), ('Technical issues', 0.07), ('Other', 0.10)]
counts = [int(round(resp * s)) for _, s in reasons]
counts[-1] += resp - sum(counts)
pd.DataFrame([dict(period='2026-07 to 2026-10', reason=n, respondents=c,
                   share_of_respondents=round(c / resp, 3), survey_invitations=canc,
                   total_respondents=resp) for (n, _), c in zip(reasons, counts)]
             ).to_csv(f'{OUT}/bramble_exit_survey.csv', index=False)

print('done')
