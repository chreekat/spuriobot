DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'ci_failure_t') THEN
        create type ci_failure_t as enum (
            'type',
            'submodule_clone',
            'MoveFileEx',
            'signal_9',
            'no_space',
            'cabal_hadrian_segfault',
            'ulimit',
            'connect_gitlab',
            'perf_note_fetch',
            'ghc-pkg_died',
            'repo_clone',
            'T16916',
            'pull_image',
            'ghc-config_file_conflict',
            'cannot_allocate',
            'job_timeout',
            'nix_T7273',
            'runner_process_terminated',
            'code_-11',
            'docker'
        );
    END IF;
END $$;

create table if not exists ci_failure (
    failure_id bigserial primary key,
    job_id bigint NOT NULL,
    type ci_failure_t NULL,
    job_date timestamp with time zone NULL,
    web_url text NULL,
    runner_id bigint NULL,
    runner_name text NULL,
    job_name text NULL,
    constraint unique_job_failure unique (job_id, type)
);
