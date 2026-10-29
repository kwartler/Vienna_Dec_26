"""
Build the Bramble+ dashboard as a single HTML file with the data embedded.

Reads the CSVs in raw_data/ and writes docs/bramble_plus/index.html.
Run from the repository root: python lessons/day1/bramble_plus/dashboard_src/build_dashboard.py
"""
import json
import os
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..', '..', '..'))
RAW = os.path.join(ROOT, 'raw_data')
OUT_DIR = os.path.join(ROOT, 'docs', 'bramble_plus')


def records(name):
    df = pd.read_csv(os.path.join(RAW, name))
    return json.loads(df.to_json(orient='records'))


cohort = pd.read_csv(os.path.join(RAW, 'bramble_cohort_retention.csv'))
adds = cohort[cohort.months_since_signup == 0][['signup_month', 'channel', 'signups']]
adds = adds.rename(columns={'signup_month': 'month', 'signups': 'new_subscribers'})

data = {
    'kpi': records('bramble_monthly_kpis.csv'),
    'channel': records('bramble_churn_by_channel.csv'),
    'plan': records('bramble_churn_by_plan.csv'),
    'region': records('bramble_churn_by_region.csv'),
    'cohort': records('bramble_cohort_retention.csv'),
    'adds': json.loads(adds.to_json(orient='records')),
    'test': records('bramble_pricing_test.csv'),
    'survey': records('bramble_exit_survey.csv'),
}
with open(os.path.join(HERE, 'europe_regions.json')) as f:
    geo = f.read()
with open(os.path.join(HERE, 'template.html'), encoding='utf-8') as f:
    html = f.read()

html = html.replace('/*__DATA__*/null', json.dumps(data, separators=(',', ':')))
html = html.replace('/*__GEO__*/null', geo)
os.makedirs(OUT_DIR, exist_ok=True)
with open(os.path.join(OUT_DIR, 'index.html'), 'w', encoding='utf-8') as f:
    f.write(html)
print('wrote', os.path.join(OUT_DIR, 'index.html'))
