# METRICS

SELECT strftime('%Y-%W', json->>'$.created_at') AS week,
       SUM(CASE WHEN (json->>'$.status') = 'success' THEN 1.0 ELSE 0 END) /
       (SUM(CASE WHEN (json->>'$.status') = 'success' THEN 1 ELSE 0 END) +
       SUM(CASE WHEN (json->>'$.status') = 'failed' THEN 1 ELSE 0 END)) ratio
FROM pipeline
where json->>'ref' = 'master'
and date('%Y-%W', json->>'created_at') >= "2023-05-16"
GROUP BY strftime('%Y-%W', (json->>'$.created_at'));


SELECT strftime('%Y-%m-%d', (json_extract(json, '$.created_at'))) AS week,
       SUM(CASE WHEN json_extract(json, '$.status') = 'success' THEN 1.0 ELSE 0 END) /
       (SUM(CASE WHEN json_extract(json, '$.status') = 'success' THEN 1 ELSE 0 END) +
       SUM(CASE WHEN json_extract(json, '$.status') = 'failed' THEN 1 ELSE 0 END))
FROM job
where json->>'ref' = 'master'
GROUP BY strftime('%Y-%m-%d', (json_extract(json, '$.created_at')));


SELECT strftime('%Y-%m-%d', json->>'created_at') AS week,
       SUM(CASE WHEN json->>'status' = 'success' THEN 1.0 ELSE 0 END) /
       SUM(CASE WHEN json->>'status' in ('failed','success') THEN 1 ELSE 0 END) AS ratio
FROM job
GROUP BY strftime('%Y-%m-%d', (json->>'created_at'));

select count(*) from job
where strftime('%Y-%W', json->>'created_at') = "2022-24"
and json->>'status' = 'success'

/******************************************
# SPURIOS
-------------------------------
*/

select json
from job
where json->>'name' like '%i386%'
limit 2

.schema

select count(*) from job_trace

select sum(pgsize) from dbstat where name = 'job_trace';

select count(*) from job_trace;

drop trigger trace_au

drop view combined;

55 gb, 38 kb, 1.5 mb per trace

drop table if exists job;
create table job (
    job_id int primary key,
    json text not null,
    created_at text generated always as (json ->> '$.created_at'),
    web_url text generated always as (json ->> '$.web_url')
    -- project_id int not null, -- generated always as (json ->> 'project_id') stored
    -- commit_id text not null,
    -- pipeline_id int not null,
    -- runner_id int,
    -- arch text not null,
    -- pipeline_source text not null,
    -- json text not null
    )
    without rowid;

-- TESTING NO-CONTENT FTS

CREATE VIRTUAL TABLE job_trace USING fts5(trace, content='');

select count(*) from job_trace where trace match '"unexpected failure for T20030_test1j(normal)"'

select count(*) from job_trace where trace match '"Command failed with error code -6"'



--select count(*) 
select job_id, json ->> '$.created_at', json 
from job
where job_id in (
    select rowid from job_trace where trace match   '"unexpected failure for jspace normal"'
)
and json ->> '$.name' not like '%thread_sanitizer%'
order by json ->> '$.created_at' desc
limit 20

create temp table ff as
select count(*) 'count', strftime("%Y-%m", json ->> '$.created_at') 'date', json -> '$.tag_list' 'tag', json ->> '$.pipeline.source' pipeline_source
from job
where job_id in (
    select rowid from job_trace where trace match '"unexpected failure for jspace normal"'
)
and json ->> '$.name' not like '%thread_sanitizer%'
-- order by json ->> '$.created_at' desc
group by strftime("%Y-%m", json ->> '$.created_at'), json -> '$.tag_list', json ->> '$.pipeline.source'
;
select * from ff
union
select sum(count) 'count', 'all' date, '*' , '*' from ff
order by date desc , tag desc, pipeline_source

create temp table jt as
select rowid from job_trace where trace match '"unexpected failure for T20030_test1j"'
;
create temp table jj as
select job_id, json ->> '$.web_url' web, json ->> '$.name' like '%thread_sanitizer' is_thread_sanitizer
from job
;
select json ->> '$.created_at', jj.job_id, jj.web
from jj
left join jt
on jj.job_id = jt.rowid
left join job on
jj.job_id = job.job_id
where 
is_thread_sanitizer = 0
and jt.rowid is not null
order by is_thread_sanitizer



create temp table jt as
select rowid from job_trace where trace match '"unexpected failure for T20030_test1j"'
;
create temp table jj as
select job_id, json ->> '$.name' like '%thread_sanitizer' is_thread_sanitizer
from job
;
select count(*), jt.rowid is null T20030_passed, is_thread_sanitizer
from jj
left join jt
on jj.job_id = jt.rowid
group by jt.rowid is null, is_thread_sanitizer
order by is_thread_sanitizer


select count(*) from job

--.mode html

.load /home/b/Downloads/sqlite3-stats
select metric, val / 60 minutes from (
select 'p99' metric, percentile_99(json ->> '$.duration') val from job 
union
select 'p95' metric, percentile_95(json ->> '$.duration') from job 
union
select 'p90' metric, percentile_90(json ->> '$.duration') from job 
union
select 'p75', percentile_75(json ->> '$.duration') from job 
union
select 'p25', percentile_25(json ->> '$.duration') from job 
union
select 'median', median(json ->> '$.duration') from job 
union
select 'mean', avg(json ->> '$.duration') from job 
union
select 'stddev', stddev_pop(json ->> '$.duration') from job 
) a

select json->> '$.created_at', json ->> '$.started_at' , json->>'$.finished_at', json->>'$.duration' / 60  from job limit 1;

select max(json->>'$.duration') / 60 from job where json->>'$.status' = 'success'


.width 8 7 30 100
.mode markdown
with
    typ as (
        select rowid, 'v_asm_a' 'typ' from job_trace where trace match 'NEAR(unexpected T5435_v_asm a)'
        union 
        select rowid, 'v_asm_b' 'typ' from job_trace where trace match 'NEAR(unexpected T5435_v_asm b)'
        union 
        select rowid, 'v_gcc' from job_trace where trace match 'NEAR(unexpected T5435_v_gcc)'
        union
        select rowid, 'dyn_asm' from job_trace where trace match 'NEAR(unexpected T5435_dyn_asm)'
    )
select
    count(distinct job_id),
    strftime('%Y-%m', json ->> 'created_at') 'month',
    group_concat(distinct typ.typ) 'types seen',
    json->>'$.name' 'job'
from job
left join typ on typ.rowid = job_id
where job_id in (
    select rowid from job_trace where trace match  'NEAR(unexpected T5435*)')
group by
    strftime('%Y-%m', json ->> 'created_at'),
    json->>'$.name'
order by strftime('%Y-%m', json ->> 'created_at') desc, count(*) desc

select job_id, json->>'$.created_at', json->>'$.duration' / 60, json->'$.runner.description', json->>'$.name'
from job
where json ->> '$.failure_reason' = 'stuck_or_timeout_failure'
order by json->>'$.created_at' desc

select count(*),
    strftime('%Y-%m', json ->> 'created_at') wen
from job
where job_id in (
    select rowid from job_trace where trace match  '"setnumcapabilities001: sendWakeup: invalid argument (Bad file descriptor)"')
group by wen
order by wen desc

select job_id, 
from job
where job_id in (
    select rowid from job_trace where trace match  '"setnumcapabilities001 [bad exit code (99): test timeout] (threaded1)"')
order by json->>'$.created_at' desc

select '* ['|| (json->>'$.name') || '](' || (json->>'$.web_url') || ')'
from job
where job_id in (
    select rowid from job_trace where trace match  '"setnumcapabilities001 [bad exit code (99): test timeout] (threaded1)"')
order by job_id desc
limit 10

select count(*), json->>'$.name' name
from job
where  name like '%sanitizer%'
group by name

select count(*), json->>'$.status'
from job
where json->>'$.name' like '%thread_sanitizer%'--  = 'nightly-x86_64-linux-deb10-validate+thread_sanitizer'




.mode markdown
.width 120 50 20
with
hadd as (
    select rowid, 'haddock' 'name' from job_trace where trace match 'NEAR("Command line" "bin/haddock")'
    union
    select rowid, 'ghc' 'name' from job_trace where trace match 'NEAR("Command line" "bin/ghc")'
)
select
    '['|| (json->>'$.name') || '](' || (json->>'$.web_url') || ')' 'job',
    json->>'$.created_at' 'date',
    hadd.name prg
from job
left join hadd on job_id = hadd.rowid
where job_id in (
    select rowid from job_trace where trace match  '"Command failed with error code: -11"')
order by job_id desc

.mode markdown
.width 2 7 100
with
hadd as (
    select rowid, 'haddock' 'name' from job_trace where trace match 'NEAR("Command line" "bin/haddock")'
    union
    select rowid, 'ghc' 'name' from job_trace where trace match 'NEAR("Command line" "bin/ghc")'
)
select count(job_id) ct, hadd.name prg, json->>'$.name' jobname
from job
left join hadd on job_id = hadd.rowid
where job_id in (
    select rowid from job_trace where trace match  '"Command failed with error code: -11"')
group by prg, jobname
order by ct desc


.mode markdown

select job_id --, json->>'web_url', date(json->>'created_at') dat, json->>'name', json->>'runner.description', json->>'status'
from job
where job_id in (
select rowid from job_trace where trace match '"The requested URL returned error:" NOT NEAR("submodule path" "failed")'
)
and json->>'status' = 'failed'
and json->>'failure_reason' = 'script_failure'
order by json->>'created_at' desc

select job_id --, json->>'web_url', date(json->>'created_at') dat, json->>'name', json->>'runner.description', json->>'status'
from job
where job_id in (
select rowid from job_trace where trace match '"error RPC failed"'
)
and json->>'status' = 'failed'
and json->>'failure_reason' = 'script_failure'
order by json->>'created_at' desc

select job_id --, json->>'web_url', date(json->>'created_at') dat, json->>'name', json->>'runner.description', json->>'status'
from job
where job_id in (
select rowid from job_trace where trace match 'NEAR("fresh repository" error, 2)'
)
and json->>'status' = 'failed'
and json->>'failure_reason' = 'script_failure'
order by json->>'created_at' desc
limit 1


.mode markdown
select json->>'web_url', date(json->>'created_at'), json->>'runner.description'
from job
where job_id in (
select rowid from job_trace where trace match '"SIGQUIT: quit"'
)
and json->>'status' = 'failed'
and json->>'failure_reason' = 'script_failure'
order by json->>'created_at' desc

.mode markdown
select json->>'web_url', date(json->>'created_at'), json->>'runner.description'
from job
where job_id in (
select rowid from job_trace where trace match '"internal error in is_section_included, at ../../gold/object.h:1286"' order by random()
)
and json->>'status' = 'failed'
and json->>'failure_reason' = 'script_failure'
order by json->>'created_at' desc

.mode markdown
select json->>'web_url', date(json->>'created_at'), json->>'runner.description'
from job
where job_id in (
select rowid from job_trace where trace match '"bad global symbol name offset" OR "internal error in is_section_included" OR "unsupported symbol binding"' order by random()
)
and json->>'status' = 'failed'
and json->>'failure_reason' = 'script_failure'
order by json->>'created_at' desc















































/**************************************
## Checking to see if we are seeing false positives about disk space outages.
*/

select count(*) , json->>'pipeline.id' -- , json->>'web_url', date(json->>'created_at') dat, json->>'name', json->>'runner.description', json->>'status'
from job
where job_id in (
select rowid from job_trace where trace match '"No space left on device" NOT "stdout hflush resource exhausted" NOT "GHC.IO.FD.fdWrite: resource exhausted"' order by rowid desc
)
and json->>'status' = 'failed'
and json->>'failure_reason' = 'script_failure'
group by json->>'pipeline.id'
order by count(*) desc


## Checking to see if there are more false positives, this time about cannot_allocate.

select job_id
/*json->>'web_url',
count(*) over (partition by json->>'pipeline.id') pipe_ct
, json->>'pipeline.web_url', date(json->>'created_at') dat, json->>'name', json->>'runner.description', json->>'status'
*/
from job
where job_id in (
select rowid from job_trace where trace match '"Cannot allocate memory" OR "out of memory allocating"' order by rowid desc
)
and json->>'status' = 'failed'
and json->>'failure_reason' = 'script_failure'
and json->>'created_at' > '2023-03-01T00:00:00+00:00'
and json->>'pipeline.id' = 64429

.mode markdown 
select json->>'pipeline.web_url', count(*)
from job
where job_id in (
select rowid from job_trace where trace match 'NEAR("stderr T18623" "failed to create OS thread")'
)
and json->>'created_at' > '2023-03-01T00:00:00+00:00'
group by json->>'pipeline.web_url'
order by count(*) desc















.schema

create table job_trace (
    job_id int not null references job(job_id),
    trace text not null
);

drop view if exists all_types;
create view all_types as
select typ, j.job_id, runner_id, arch "tag", pipeline_source, created_at, web_url
from job j
join (
    select 'docker' typ, rowid as "job_id"
        from fts_job_trace
        where trace match '"Cannot connect to the Docker daemon at unix:///var/run/docker.sock"'
    union
    select 'no_space' typ, rowid as "job_id"
        from fts_job_trace
        where trace match '"No space left on device"'
    union
    select 'connect_gitlab' typ, rowid as "job_id"
        from fts_job_trace
        where trace match '"Failed to connect to gitlab.haskell.org"'
    union
    select 'pull_image' typ, rowid as "job_id"
        from fts_job_trace
        -- where trace regexp 'failed to pull image "registry.gitlab.haskell.org'
        where trace match '"failed to pull image registry.gitlab.haskell.org"'
    union
    select 'signal_9' typ, rowid as "job_id"
        from fts_job_trace
        where trace match '"failed due to signal 9"'
    union
    select 'T16916' typ, rowid as "job_id"
        from fts_job_trace
        where trace match '"Idle CPU consumption too different"'
    union
    select 'cannot_allocate' typ, rowid as "job_id"
        from fts_job_trace
        where trace match '"Cannot allocate memory"'
        or trace match '"osCommitMemory: VirtualAlloc MEM_COMMIT failed"'
    union
    select 'MoveFileEx' typ, rowid as "job_id"
        from fts_job_trace
        where trace match '"MoveFileEx"'
) type
on type.job_id = j.job_id
;
drop view if exists summary;
create view summary as
select typ, count(*) num, runner_id, tag, pipeline_source
from all_types
group by typ, runner_id, tag, pipeline_source
order by num desc
;
select * from summary




create temp table jj as 
select * from job
where job_id in (
    select rowid from job_trace where trace match '"ghc-pkg dump failed: dieVerbatim: user error"'
)
;
select count(*), '*' from jj
union
select count(*),  json ->> '$.runner.description'
from jj
group by  json ->> '$.runner.description'
order by count(*) desc

