"""
Synthetic data generator for the Bramble+ dashboard.

Produces monthly subscription metrics (November 2024 to October 2026)
and supporting tables as CSV files in this folder.

Run: python synthetic_data.py
"""
import numpy as np
import pandas as pd

RNG = np.random.default_rng(2026)
OUT = '.'

N = 24
MONTHS = [f'{2024 + (10 + i) // 12}-{(10 + i) % 12 + 1:02d}' for i in range(N)]   # 2024-11 .. 2026-10
PLANS = ['monthly', 'annual']
CHANNELS = ['organic', 'paid_search', 'referral', 'influencer']
REGIONS = ['DACH', 'Nordics', 'Benelux', 'Southern Europe']
PRICE = {'monthly': 12.99, 'annual': 129 / 12}

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
START_SUBS = 1_768_000
START_MONTHLY_SHARE = 0.7034
BASE_CHANNEL_SHARE = np.array([0.50, 0.32, 0.18])
NEW_CHANNEL_SHARE = np.array([0.50, 0.32, 0.18])
NEW_ANNUAL_SHARE = 0.22

REGION_SHARE = {                      # by channel
    'organic':     [0.40, 0.22, 0.18, 0.20],
    'paid_search': [0.38, 0.22, 0.20, 0.20],
    'referral':    [0.44, 0.20, 0.18, 0.18],
    'influencer':  [0.62, 0.12, 0.12, 0.14],
}

# monthly cancellation rate
CHURN_RATE = np.array([0.0406, 0.0397, 0.0412, 0.0418, 0.0395, 0.0403,
                       0.0421, 0.0400, 0.0393, 0.0409, 0.0415, 0.0402,
                       0.041, 0.044, 0.043, 0.041, 0.040, 0.041,
                       0.039, 0.039, 0.044, 0.045, 0.046, 0.046])
STATUS_CHANGE_MONTH = 18
PAUSE_SHARE = 0.085

BASE_ADDS = np.array([73.2, 75.7, 76.2, 71.7, 72.7, 76.7, 74.2, 71.2, 75.2, 76.7, 73.7, 72.0, 63.9, 66.4, 76.4, 64.4, 63.4, 62.4, 60.4, 79.0, 65.8, 69.8, 68.8, 66.8]) * 1000
PARTNER_ADDS = {16: 74_000, 17: 63_000, 18: 41_000}
INTRO_MONTHS = 3

# relative cancellation propensities
CH_REL = np.array([1.0, 1.12, 0.82])
PLAN_REL = {'monthly': 1.0, 'annual': 0.29}
NEW_SUB_REL = 1.2                     # first two months
PARTNER_HAZARD = [0.025, 0.025, 0.025, 0.24, 0.145, 0.095]
PARTNER_LATER = 0.075

START_TENURE_MEAN = 5.800
MAXAGE = 90
CAC = np.array([40.1, 39.2, 38.6, 39.8, 40.4, 39.0, 38.3, 39.6, 40.8, 39.4, 38.9, 39.9,
                39.5, 40.2, 37.8, 38.9, 33.1, 33.8, 35.0, 39.0, 39.8, 39.1, 38.4, 38.1])

ages = np.arange(1, MAXAGE + 1)


def age_weights(mean):
    q = 1 - 1 / mean
    for _ in range(60):
        w = q ** (ages - 1)
        w /= w.sum()
        q = q + (mean - (w * ages).sum()) * 0.002
    return w


def tenure_now(base, cohort, t):
    b = (base.sum(axis=(0, 1)) * ages).sum()
    c = (cohort.sum(axis=(0, 1)) * np.clip(t - np.arange(N), 0, None)).sum()
    return (b + c) / (base.sum() + cohort.sum())


def paying_mrr(base, cohort, t):
    mrr = 0.0
    for pi, pl in enumerate(PLANS):
        mrr += base[pi].sum() * PRICE[pl]
        for k in range(N):
            for ci in range(4):
                n = cohort[pi, ci, k]
                if n <= 0:
                    continue
                price = PRICE[pl]
                if ci == 3 and k in PARTNER_ADDS and 0 <= t - k < INTRO_MONTHS:
                    price = PRICE[pl] * 0.5
                mrr += n * price
    return mrr


# ---------------------------------------------------------------------------
# Simulation
# ---------------------------------------------------------------------------
w = age_weights(START_TENURE_MEAN)
base = np.zeros((2, 3, MAXAGE))       # plan x channel x age
for ci in range(3):
    base[0, ci] = START_SUBS * BASE_CHANNEL_SHARE[ci] * START_MONTHLY_SHARE * w
    base[1, ci] = START_SUBS * BASE_CHANNEL_SHARE[ci] * (1 - START_MONTHLY_SHARE) * w
cohort = np.zeros((2, 4, N))          # plan x channel x signup month
paused = np.zeros((2, 4))
records = []

for t in range(N):
    start_active = np.zeros((2, 4))
    start_active[:, :3] = base.sum(axis=2)
    start_active += cohort.sum(axis=2)
    start_total = start_active.sum() + paused.sum()
    start_by = start_active + paused

    target_cancels = CHURN_RATE[t] * start_total
    status_adj = 1.0 / (1 - PAUSE_SHARE) if t >= STATUS_CHANGE_MONTH else 1.0

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
    relc = np.zeros((2, 3, N))
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
        cancels = exits

    nb = np.zeros_like(base)
    nb[:, :, 1:] = base[:, :, :-1]
    nb[:, :, -1] += base[:, :, -1]
    base = nb

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
                        cancels=cancels, adds=cohort[:, :, t].copy(),
                        end_active=end_active, paused=paused.copy(),
                        tenure=tenure_now(base, cohort, t), mrr=paying_mrr(base, cohort, t),
                        cohort=cohort.copy()))

# ---------------------------------------------------------------------------
# Monthly KPIs
# ---------------------------------------------------------------------------
kpi = []
for r in records:
    t = r['t']
    subs = r['end_active'].sum() + r['paused'].sum()
    paying = r['end_active'].sum()
    adds = r['adds'].sum()
    spend = adds * CAC[t] * RNG.normal(1, 0.005)
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
REGION_RNG = np.random.default_rng(11)
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
    s_rg = np.array([sum(sb[:, ci].sum() * REGION_SHARE[c][ri] for ci, c in enumerate(CHANNELS)) for ri in range(4)])
    x_rg = np.array([sum(rp[:, ci].sum() * REGION_SHARE[c][ri] for ci, c in enumerate(CHANNELS)) for ri in range(4)])
    x_adj = x_rg * REGION_RNG.normal(1, 0.02, 4)
    x_adj *= x_rg.sum() / x_adj.sum()
    for ri, rg in enumerate(REGIONS):
        rows_rg.append(dict(month=MONTHS[t], region=rg, subscribers_start=round(s_rg[ri]),
                            cancellations=round(x_adj[ri]), churn_rate=round(x_adj[ri] / s_rg[ri], 4)))
pd.DataFrame(rows_ch).to_csv(f'{OUT}/bramble_churn_by_channel.csv', index=False)
pd.DataFrame(rows_pl).to_csv(f'{OUT}/bramble_churn_by_plan.csv', index=False)
pd.DataFrame(rows_rg).to_csv(f'{OUT}/bramble_churn_by_region.csv', index=False)

# ---------------------------------------------------------------------------
# Cohort retention
# ---------------------------------------------------------------------------
rows_co = []
for k in range(N):
    for ci, c in enumerate(CHANNELS):
        n0 = records[k]['adds'][:, ci].sum()
        if n0 <= 0:
            continue
        for t in range(k, N):
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
canc = int(round(sum(r['cancels'].sum() for r in records[20:24])))
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
