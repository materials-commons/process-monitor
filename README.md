process-monitor
===============
The process monitor will monitor a set of external (outside of Erlang) processes
and restart them if nescessary. The process monitor uses erlang ports (open_port)
to start external processes.

The system reads the file pm.config to configure the processes and supervision strategy.
The pm.config file is located in the INSTALL_DIR/etc directory. The file has the following
format:

```erlang
{SupervisorGroupName, RestartSpec, [JobSpec]}.
```

You can have multiple entries in the file.

Where:
SupervisorGroupName is an atom(). This atom will be appended to the pm_sup and must
be unique.

RestartSpec is a Supervisor RestartSpec.

JobSpec is {JobName, "Number Of Processes to create", "Process to Run"}, eg
```erlang
{metadata_jobs, 5, "/usr/local/bin/metadata_parser -s"}
```

The JobName must be an atom() and must be unique within a particular Supervisor Group. The
job name is appended to pm_server along with an incrementing count for each process.

For example, given an entry like:
```erlang
{metadata_processing, {one_for_one, 500, 10},
    [{extract_metadata, 3, "/usr/bin/metadata_parser -s"},
     {index_md, 2, "/usr/bin/md_indexer -f /tmp"}]}.
```

This will start 3 /usr/bin/metadata_parser commands, and 2 /usr/bin/md_indexer commands.
Inside the Erlang VM it will create one supervisor named 'pm_sup_metadata_processing',
and 5 workers named "pm_server_extract_metadata_1", "pm_server_extract_metadata_2",
"pm_server_extract_metadata_3", "pm_server_index_md_1", and "pm_server_index_md_2".

The supervisor pm_sup_metadata_processing will have {one_for_one, 500, 10} as its restart 
spec.

You can have multiple entries in the file. For example:

```erlang
{metadata_processing, {one_for_one, 500, 10},
    [{extract_metadata, 3, "/usr/bin/metadata_parser -s"},
     {index_md, 2, "/usr/bin/md_indexer -f /tmp"}]}.

{text_extracter, {one_for_all, 20, 5},
    [{step_1, 2, "/usr/local/bin/extracter -q Extracter"},
     {step_2, 5, "/usr/local/bin/loader -q Loader -f /tmp"}]}.
```

This will create two supervisor groups. Each of the separate supervisors will manage
their respective processes in a different manner.
