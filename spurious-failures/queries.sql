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
select json ->> '$.created_at', json ->> '$.pipeline.source'
from job
where job_id in (
        select rowid from job_trace where trace match '"unexpected failure for jspace normal"' order by rowid desc
)

create temp table ff as
select count(*) 'count', strftime("%Y-%m", json ->> '$.created_at') 'date', json -> '$.tag_list' 'tag', json ->> '$.pipeline.source' pipeline_source
from job
where job_id in (
    select rowid from job_trace where trace match '"unexpected failure for jspace normal"'
)
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
select job_id, json ->> '$.name' like '%thread_sanitizer' is_thread_sanitizer
from job
;
select json ->> '$.created_at', jj.job_id, jt.rowid is null T20030_passed, is_thread_sanitizer
from jj
left join jt
on jj.job_id = jt.rowid
left join job on
jj.job_id = job.job_id
where 
is_thread_sanitizer = 0
and T20030_passed = 0
order by is_thread_sanitizer
















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




