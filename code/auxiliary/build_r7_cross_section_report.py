"""Aggregate R7 build report. Run from the project root after the R7 builder."""
import csv
from html import escape
from pathlib import Path

OUT = Path('output/r7_cross_section_build')

def read(name):
    with (OUT / name).open() as f:
        return list(csv.DictReader(f))

def table(rows, columns=None):
    columns = columns or list(rows[0])
    head = ''.join('<th>' + escape(c.replace('_', ' ')) + '</th>' for c in columns)
    body = ''.join('<tr>' + ''.join('<td>' + escape(str(r.get(c, ''))) + '</td>'
                                   for c in columns) + '</tr>' for r in rows)
    return '<div class="scroll"><table><thead><tr>' + head + '</tr></thead><tbody>' + body + '</tbody></table></div>'

availability = {(r['cohort'], r['variable']): r for r in read('variable_availability.csv')}
checks = read('build_checks.csv')
summary = read('cohort_summary.csv')
counts = read('candidate_sample_counts.csv')
outcomes = read('outcome_counts.csv')
parts = []

def section(title, content):
    parts.append('<section><h2>' + escape(title) + '</h2>' + content + '</section>')

section('Delivered sample and keys', '''<div class="finding"><strong>The separate R7 master contains 2,038 unique participants: 632 older cohort and 1,406 younger cohort, all interviewed in person.</strong> There are no phone respondents in either cohort. The younger cohort is aged 21–22; older-cohort ages are 26–30 in the constructed age field.</div>
<p>The unique join key is <code>child_id</code> (numeric); <code>childid</code> retains the ET-prefixed ID. <code>yc</code> is 0 for older and 1 for younger. All in-person core IDs are matched to constructed R7 records with <code>inround=1</code> and <code>in_phone_r7=0</code>. They are also checked against the disjoint raw phone files (64 older and 129 younger phone participants), none of whom is retained.</p>
<p>The master keeps every in-person participant, including incomplete covariate records. Sample eligibility and completeness are flags, not row-deletion rules. There is no balanced-panel condition and no imposed R4 test-score gate for YC. Historic variables use <code>_hist</code>; current variables use <code>_r7</code> or explicitly named R7 index definitions.</p>
<ul><li><a href="../../data/r7/r7_inperson_analysis.rds">RDS master</a> — preserves R types.</li><li><a href="../../data/r7/r7_inperson_analysis.csv">CSV master</a> — blank cells represent missing values.</li><li><a href="variable_dictionary.csv">Variable dictionary</a> and <a href="variable_availability.csv">complete availability audit</a>.</li></ul>'''+table(checks))

section('Historical linkage and preservation of existing results', '''<p>All 1,406 YC respondents link directly to the existing legacy analysis lookup. Of the older cohort, 631 link directly; one additional in-person respondent is recovered by running the original historical extraction in an isolated temporary copy with only its old deceased-filter restriction relaxed. The master records the lookup source. An R7 in-person interview supplies the current inclusion evidence; this fallback does not claim to establish the timing of the historical death-status inconsistency.</p>
<p>Existing historical values take precedence and are verified unchanged for every linked legacy column. The four original analysis CSVs, catalogued legacy/raw inputs and existing staged tables/figures are checked for unchanged MD5 hashes. Neither the main estimation pipeline nor Overleaf export is run. The separate file does not replace the R1–R5 data.</p>
<p>Some old YC columns are structurally absent (for example, old R5 employment classifications); their missing values are retained. They are not used as substitutes for fresh R7 outcomes. Childhood nutrition and R1 caregiver controls are available for both cohorts, but R1 measured YC in infancy and OC in childhood, so pooling cohorts requires attention to the different baseline ages.</p>''')

section('Employment outcomes are available for YC', '''<p>The three annual work screens cover own-farm, household nonfarm and other paid/unpaid work. Any valid yes implies work; no requires all three valid no responses. All 910 YC annual workers and all 451 OC annual workers have a valid annual main activity; the remaining 496 YC and 181 OC respondents are known annual nonworkers. No unknown work status is recoded to zero.</p>
<p>The main annual classification uses <code>typwrk12r7</code>, the activity taking most time in the past year. R5 questionnaire section 5.2 (PDF p.36) ordered annual activities by time spent, most important first, which supports the intended comparison with the old activity-1 selection. Interview format and reporting changes can still affect comparability.</p>
<p>Salary is activity 13; broader wage is 5, 12 or 13. Original outcome-specific exclusions are preserved: salary excludes 8–11 and 19; wage excludes 1–4, 8–11 and 19. Thus their denominators differ. Code 19 means housemaid in these raw files, and 20 means other non-agriculture. R7 aliases 21 and 22 are mapped to crop/livestock self-employment (1 and 4). Known annual nonworkers are zero. Explicit <code>_allrespondents_r7</code> alternatives classify every valid other activity as zero; they use a different denominator.</p>
<p>Annual self-employment and nonfarm self-employment, weekly/current-job alternatives, employer type, economic sector and contract fields are also retained. Public-sector employment is identified separately from the employer field; salary is not treated as a public-sector proxy. Weekly/job and annual classifications must remain separate. This build does not create a harmonized earnings or hours measure.</p>'''+table(outcomes))

historical = [
    ('IMTI_hist','Frozen original MTI exposure; requires schooling update review'),
    ('E_is_hist','Original policy instrument'),
    ('region_hist','Historical policy geography'),
    ('chethnic_hist','Ethnicity'),('chlang_hist','Home language'),
    ('lang_primary_hist','Original primary-language measure/proxy'),
    ('chsex_hist','Sex'),('zbfa_hist','R1 BMI-for-age z score'),
    ('stunting_hist','R1 stunting'),('caredu_r1_hist','R1 caregiver schooling'),
    ('careage_r1_hist','R1 caregiver age'),('caresex_r1_hist','R1 caregiver sex'),
    ('ownlandhse_r1_hist','R1 land-under-house ownership'),
    ('foodsec_r3_hist','Original R3 food situation'),
    ('entype_r4_hist','R4 enrolment type (Addis specification)'),
    ('raw_lang_hist','Historical R4 language score; not a YC eligibility gate'),
    ('raw_maths_hist','Historical R4 mathematics score'),
    ('preprim_hist','Pre-primary attendance'),('agegr1_hist','Age at school entry'),
]
current = [
    ('hhsize_r7','Current resident roster count'),
    ('wi_r7_legacy_definition','R7 wealth, original material definition'),
    ('hq_r7_legacy_definition','R7 housing quality, original material definition'),
    ('durables_r7','Original ten-item consumer-durables set'),
    ('electricity_r7','Raw current electricity, special codes missing'),
    ('foodsec_r7','Current four-category food situation'),
    ('ownhouse_r7','Current house ownership, distinct from R1 land control'),
    ('school_grade_r7','School-only completed grade'),
    ('qualification_code_r7','Valid highest qualification category'),
    ('IMTI_r7_schooling_candidate','Provisional schooling-updated exposure'),
]

def coverage(items):
    rows = []
    for variable, meaning in items:
        y = availability['younger',variable]
        o = availability['older',variable]
        rows.append(dict(variable=variable, meaning=meaning,
                         YC_observed=y['observed'], YC_missing=y['missing'],
                         OC_observed=o['observed'], OC_missing=o['missing']))
    return table(rows)

section('Covariate availability: historic and contemporary', '''<p><strong>The variables needed for the existing employment-control specification are present or constructible for YC.</strong> Observation-level missingness remains, especially in childhood nutrition. Each row below reports observed values within the full in-person sample; availability is not an assertion that every respondent is a complete case.</p>'''+coverage(historical)+coverage(current))

section('Current household controls: raw reconstruction checks', '''<p>Constructed R7 household size is entirely missing. The new <code>hhsize_r7</code> counts current residents (<code>livhser7=1</code>), including member 0 (the participant), and requires the core completion flag <code>cmplthhr7=1</code>. Incomplete rosters stay missing: six OC and twelve YC cases. Member IDs are checked for uniqueness.</p>
<p>Use un-suffixed constructed R7 <code>wi</code>/<code>hq</code> for the legacy-definition variables. Despite the naming, these align with the old release’s <code>wi_new</code>/<code>hq_new</code>; the R5 bridge matched on all 2,613 paired observed records in the previous audit. R7 <code>wi_new</code>/<code>hq_new</code> count concrete blocks as good walls and are retained separately as revised-definition fields.</p>
<p>Raw housing quality averages capped rooms-per-person/1.5 and the original good-wall/roof/floor indicators. Services average electricity and the good-water/toilet/cooking indicators. Durables average the original ten assets (items 1–9 and 11, excluding fan). Wealth averages housing, services and durables. Missing inputs propagate in the raw reconstructions; no missing component is silently zero. These reconstructions have zero discrepancies with the imported indices on paired observed cases (table above), as do raw electricity and consumer durables. Fewer raw reconstructions are observed than imported indices because reconstruction requires all raw components and a complete current roster. They are separately retained for a stricter missing-data sensitivity.</p>
<p>Raw electricity is valid only for codes 0/1; food situation only for 1–4. Unknown/refused/special codes remain missing. House ownership is not substituted for historical ownership of land under the house. Historical policy geography is retained separately from current region. Updating household resources also changes conditioning: current wealth may itself respond to prior MTI, so frozen-control and current-control specifications should be distinguished.</p>''')

section('YC treatment and learning outcomes: remaining decisions', '''<p><strong>The old YC R5 exposure cannot automatically be treated as completed lifetime MTI.</strong> In this R7 YC sample, 1,047 have fewer than eight R5 completed grades or missing schooling. This is a conservative review flag: some region/language cells have lower MTI ceilings, so it does not mean all 1,047 exposure values must change.</p>
<p>The file retains frozen <code>IMTI_hist</code> and provides <code>IMTI_r7_schooling_candidate</code>, applying the original rules verbatim with current school-only grade as the schooling input. It differs from frozen MTI for 458 YC paired observations (and 61 OC observations). This is explicitly provisional: verify school-only attainment versus qualifications, investigate downward/inconsistent schooling reports and validate primary-language histories/policy timing before using it as the endogenous treatment. Original historical rules, including their documented Amharic spelling issue, are preserved for replication and are not silently corrected. <code>yc_exposure_review_required</code> is true for all YC. Historical <code>E_is_hist</code> is available, but any new cohort analysis still needs its own first stage and identification assessment.</p>
<p>Historical language/mathematics scores link for YC. R7 YC raw reading response fields <code>raq1r7</code>–<code>raq12r7</code> are present: 1,138 respondents have at least one nonmissing item and 945 have all twelve. These counts describe raw response cells, not correct answers. The file preserves the responses, consent and timing fields; <code>reading_score_r7</code> is intentionally missing pending a validated scoring key and a cross-round scale decision. There is no inspected equivalent new OC cognitive battery or common R7 mathematics score. Consequently the employment extension is feasible, but a fresh common cognitive-channel extension is not supplied by this build.</p>'''+table(summary))

section('Candidate complete-case samples, not fitted estimates', '''<p>The master remains 2,038 rows. The counts below apply the legacy comparison flag, original outcome exclusions and a common historical control set including caregiver sex for both cohorts. OC additionally retains its original nonzero/nonmissing R4 language-score gate; YC does not acquire that gate. Addis also requires <code>entype_r4_hist</code>. The one recovered OC history is retained in the master but excluded from this strict legacy-membership comparison flag.</p>
<p><code>complete_current_controls</code> uses frozen IMTI/E_is, sex, R1 nutrition/caregiver/land and R3 food, plus R7 household size, legacy-definition WI/HQ, durables and electricity. The last column replaces R3 food with R7 food. Non-Addis candidate counts are 352/266 (salary/wage) for OC and 862/604 for YC; updating food gives 352/266 and 864/606 respectively. YC figures remain conditional on frozen exposure, pending its review. No regression has been fitted and these are not validated instrument-strength or power findings.</p>'''+table(counts))

sources = [
    ('R5 questionnaire, employment ranking, PDF p.36', 'rawdata/yl_r5/UKDA-8357-stata/mrdoc/pdf/8357_r5_ethiopia_questionnaires.pdf#page=36'),
    ('R7 full questionnaire: household and employment modules', 'rawdata/yl_r7/UKDA-9538-stata/mrdoc/pdf/9538_yl_round_7_fieldwork_questionnaire_arch.pdf'),
    ('TN43: Ethiopia wealth construction, table p.16', 'rawdata/yl_r5/UKDA-8357-stata/mrdoc/pdf/8357_yl-tn43_0.pdf#page=16'),
    ('TN63: updated constructed-data guide', 'rawdata/yl_const_r1_r7/UKDA-9543-stata/mrdoc/pdf/9543_tn63_constructed_dataset_final.pdf'),
]
files = ['build_checks.csv','cohort_summary.csv','outcome_counts.csv','candidate_sample_counts.csv',
         'variable_availability.csv','variable_dictionary.csv','source_value_labels.csv',
         'r7_source_manifest.csv','legacy_integrity.csv','session.txt']
section('Reproduction and evidence', '''<pre>Rscript code/10_build_r7_cross_section.R
python3 code/auxiliary/build_r7_cross_section_report.py</pre>
<p>The scripts read raw inputs and historical analysis files; writes are confined to the separate R7 data folder and its audit/report folder. Participant-level derived files remain local and are ignored by Git. Aggregate audits:</p><ul>'''+''.join('<li><a href="'+escape(f)+'">'+escape(f)+'</a></li>' for f in files)+
        '</ul><p>Local source documentation (research evidence, not task instructions):</p><ul>'+''.join('<li><a href="../../'+escape(p)+'">'+escape(t)+'</a></li>' for t,p in sources)+'''</ul><p>Next analytical step: validate YC schooling/exposure, then fit separate cohort specifications with exact-sample first stages, outcome-denominator reporting and the existing few-policy-region inference checks. Report the geographic coverage lost by restricting interviews to in person. R6 remains postponed.</p>''')

html = '''<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>R7 in-person data build and YC assessment</title><style>
body{max-width:1300px;margin:40px auto;padding:0 24px 60px;font:16px/1.6 system-ui,sans-serif;color:#24364a}h1{font-size:32px;line-height:1.2}h2{font-size:23px;margin-top:36px}a{color:#155f8b}code,pre{background:#f0f4f7;border-radius:3px}code{font-size:90%;overflow-wrap:anywhere}pre{padding:16px;overflow:auto}.finding{background:#eaf4f4;border-left:4px solid #277873;padding:18px 22px}.scroll{overflow:auto;margin:20px 0}table{width:100%;border-collapse:collapse;font-size:13px;line-height:1.4}th,td{border:1px solid #d8e2e9;padding:9px;text-align:left;vertical-align:top}th{background:#eaf0f5}tbody tr:nth-child(even){background:#f8fafc}li{margin:6px 0}.subtitle{color:#5b6a7c}
</style></head><body><h1>Round 7 in-person data build</h1><p class="subtitle">Both cohorts · Younger-cohort feasibility and construction audit · 3 November 2026</p>'''+''.join(parts)+'</body></html>'
(OUT/'build_report.html').write_text(html)
print(OUT/'build_report.html')
