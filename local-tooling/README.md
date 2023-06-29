# Spurio

Tools for inspecting (GHC) GitLab job logs.

## `fetch-job-data`

### Synopsis

```
    ./result/bin/fetch-job-data [BEGIN_DATE [END_DATE]]
```

`fetch-job-data` will fetch job logs from completed jobs.

The tool can fetch jobs from a date range. The default range is the last month.

### Usage Example

*Fetch all data since 1 November 2022*:

```
./result/bin/fetch-job-data 2022-11-01T00:00:00+00:00
```

*Fetch all data since last Monday*:

```
./result/bin/fetch-job-data "$(date -Isec -d 'last Monday')"
```

### Usage Notes

The project url is hard-coded for GHC at the moment.

It will fetch 10 job logs at a time in order to speed things up.
You can control this with `+RTS -N<num>`.
Try to be nice. :)

### Data Notes

Data is put into a SQLite database called `./jobs.db`, which will be initialized
automatically. Example usage:

```sql
select json ->> '$.created_at', json ->> '$.pipeline.source'
from job
where job_id in (
    select rowid from job_trace
    where trace match '"unexpected failure for jspace normal"'
    order by rowid desc
)
```

SQLite full-text search reference: https://www.sqlite.org/fts5.html

### How to build the tool

`nix-build`
