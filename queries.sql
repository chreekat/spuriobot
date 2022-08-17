create table job (
    job_id int primary key,
    project_id int not null,
    commit_id text not null,
    pipeline_id int not null,
    runner_id int,
    arch text not null,
    pipeline_source text not null,
    created_at text not null,
    web_url text not null);
create table job_trace (
    job_id int not null references job(job_id),
    trace text not null
);

-- TAKES TWO HOURS!
-- drop table fts_job_trace;
-- create virtual table fts_job_trace using fts5(job_id, trace);
-- insert into fts_job_trace select * from job_trace

create temporary table have_trace (job_id primary key);
insert into have_trace select job_id from fts_job_trace;
insert into fts_job_trace
select t.job_id, t.trace
from job_trace t
left join have_trace have
on t.job_id = have.job_id
where have.job_id is null;

drop view if exists combined;
create view combined as
select j.job_id, runner_id, arch "tag", pipeline_source, created_at, web_url, trace
from fts_job_trace t
join job j
on t.job_id = j.job_id
;
drop view if exists all_types;
create view all_types as
select 'docker' typ, *
    from combined
    where trace match '"Cannot connect to the Docker daemon at unix:///var/run/docker.sock"'
union
select 'no_space' typ, *
    from combined
    where trace match '"No space left on device"'
union
select 'connect_gitlab' typ, *
    from combined
    where trace match '"Failed to connect to gitlab.haskell.org"'
union
select 'pull_image' typ, *
    from combined
    -- where trace regexp 'failed to pull image "registry.gitlab.haskell.org'
    where trace match '"failed to pull image registry.gitlab.haskell.org"'
union
select 'signal_9' typ, *
    from combined
    where trace match '"failed due to signal 9"'
union
select 'T16916' typ, *
    from combined
    where trace match '"Idle CPU consumption too different"'
union
select 'cannot_allocate' typ, *
    from combined
    where trace match '"Cannot allocate memory"'
-- union
-- select 'segfault' typ, *
--     from combined
--     where trace match '"Segmentation fault"'
;
drop view if exists summary;
create view summary as
select typ, count(*) num, runner_id, tag, pipeline_source
from all_types
group by typ, runner_id, tag, pipeline_source
order by num desc
;
select * from summary
