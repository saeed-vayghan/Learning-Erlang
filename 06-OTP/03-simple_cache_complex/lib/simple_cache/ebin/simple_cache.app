%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
%% This is the application resource file for simple_cache

{application, simple_cache,
 [{description, "A simple caching system"},
  {vsn, "0.1.0.3"},
  {modules, [simple_cache,
             sc_app,
             sc_sup,
             sc_element_sup,
             sc_store,
             sc_element,
             sc_event,
             sc_event_logger]},
  {registered, [sc_sup]},
  {applications, [kernel, sasl, stdlib]},
  {mod, {sc_app, []}}
 ]}.
