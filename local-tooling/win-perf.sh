#!/usr/bin/env nix-shell
#! nix-shell -p parallel -i bash

tmp=$(mktemp -d)
trap "rm -rf $tmp" EXIT

commands=${tmp}/commands
test_name_dat=${tmp}/test_name_dat
test_names=${tmp}/test_names
test_types=${tmp}/test_types

# For each failed windows job in the last couple weeks...
PGUSER=bryan PGDATABASE=gitlab PGHOST=~/HaskellFoundation psql -tc \
    "select id from ci_builds job where job.name like '%windows%' and project_id = 1 and started_at is not null and status = 'failed' and started_at > '2023-07-21' order by job.started_at desc" | grep -v '^\s*$' | while read id; do
    log="${id}.log"
    if [ ! -e "$log" ]; then
        echo Fetch $id
        echo "./trace $id > $log" >> $commands
    else
        echo Have $id
    fi
done

exit 0
# ...fetch the missing traces.
parallel -j 5 < $commands

# Extract the names of failed stat tests.
#
# Looks like: TEST="T4813 TcPlugin_RewritePerf hard_hole_fits"
grep -hao '^TEST="[^"]*"' *.log > $test_name_dat
sed -e 's/TEST="//' -e 's/"$//' -i $test_name_dat
tr ' ' '\n' < $test_name_dat | sort -u | tr '\n' '|' | sed -e 's/|$//' > $test_names

# Grep the source tree and find out what kind of stat test they are.
cat $test_names | xargs -I{} --  rg -A3 {} ~/HaskellFoundation/clones/ghc/testsuite/tests/ | grep -B3 collect_ >> $test_types

echo -n "collect_stats: "
grep collect_stats $test_types | wc -l

echo -n "collect_compiler*: "
grep collect_compiler $test_types | wc -l
