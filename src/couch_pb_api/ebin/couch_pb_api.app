%%-*- mode: erlang -*-

{application, couch_pb_api,
 [
  {description, ""},
  {vsn, ""},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []},
  {mod, {couch_pb_api_app, []}},
  {modules, [couch_pb_api_app, couch_pb_api_sup, couch_pb_api_listener,
             couch_pb_api_server]}
 ]}.
