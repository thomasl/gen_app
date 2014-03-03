NOTE: You may have to edit the src/Makefile mildly to build.
For example: Delete the parse\_transform line from ERLC\_OPTS, since
it requires the smart\_exceptions parse transform. Change paths
BASE\_DIR and UTIL\_DIR. Sorry.

Comments are welcome. See end of file for pull requests
and future work.

Introduction
============

Standard Erlang/OTP implements the application/supervisor
framework using erlang modules, which in practice often are
boilerplate code. You write a foo.app file, which starts a
foo_app.erl module, which in turn invokes
a foo_sup.erl supervisor module which starts up and maintains
a supervisor tree.

Gen\_app is intended to get rid of this boilerplate in what
are the most common use cases: an application starts a supervisor
tree according to some fixed scheme. We replace this by passing
a data structure to the gen\_app or gen\_sup module, which
specifies what is to be started and how.

Capabilities
----------

- Can start application statically from .app file or dynamically from
  function call.
- Can start supervisor tree with gen\_sup inside ordinary
  application module.
- Sensible defaults provided.
- Simple to start application which starts supervisor with
  fixed set of children. (Most common use case.)
- Simple to start application which starts supervisor with
  dynamically added children.
- Simple to start distributed application, dynamically or statically.
- Simple to start recursive tree of supervisors.
- Simple syntax for starting servers as children.
- Simple syntax for starting raw processes using supervisor\_bridge.
- Simple syntax for starting and maintaining ets tables under
  supervision, eliminating extra boilerplate processes.
- Powerful specifications with full OTP capabilities available
  when needed.
- Environment key syntax provided to further simplify
  and power up specifications.
- Provided as code application. (Applications using gen\_app
  should depend on gen\_app.)

Limitations
----------

- The 'simple\_one\_for\_one' supervisor strategy has not been
  tested (Q: is this an important use case?)
- Callback config_change/3 just returns. Due to the callback API, we can't do anything app specific at this time.
- The application key 'start\_phases' is not supported. If you need this functionality, it makes sense to instead write an ordinary application module.
- Applications always start as 'temporary', which is the common use case. There is currently no way to specify 'permanent' or 'transient'. (Q: do you need these startup types?)

Usage
=====

You can use gen\_app interactively or in an .app file.

Dynamic startup
-----------
Here is the possibly simplest example.

    > gen_app:start(testapp, [{sup, my_sup}]).

This starts an application testapp consisting of a single
supervisor my\_sup.
Children can then be added dynamically using the gen\_sup:start\_child/2
call.

    > gen_sup:start_child(my_sup, my_gen_server).

This starts my\_gen\_server:start\_link() as a supervised child of my\_sup.
You can also start the child directly as part of the gen\_app:app\_sup/2
call. See below for more information.

Static startup
----------

Using gen\_app in an .app file is easy: just invoke gen\_app
with a supervisor specification. The supervisor specifications
are further defined below, but we go through some simple
examples here.

The following starts a supervisor my\_sup without children.

    {application, my_app,
      [{mod, {gen_app, [{sup, my_sup}]}}]}.

The following starts a supervisor my\_sup with a child started
as my\_server:start\_link().

    {application, my_app,
      [{mod, {gen_app, [{sup, my_sup, [{my_server, []}]}]}}]}.

Note that gen\_app only handles the common case of starting a
supervisor tree. If your application needs start\_phases, handling
of config\_change, or suchlike, you will need to write a custom
application module. 

(You may then still be able to use gen\_sup without
gen\_app.)

Application specifications
--------------------------

You specify an application as follows.

    gen_app:start(App, AppKeys)

where AppKeys is a list of key-value pairs specifying the
application. (See the ['application' module man page](http://www.erlang.org/doc/apps/kernel/application.html) for what keys are available.) 

You _must_ use key 'sup' or 'supervisor'
to specify the supervisor tree to be started. More complex cases
currently have to be handled in standard Erlang/OTP outside of the
gen\_app framework.

In addition to the standard keys, you can also specify that the application starts up distributed,
using the key 'distributed'. The allowed values of 'distributed' are:

    local
    distributed
    {distributed, Nodes}
    {distributed, Time, Nodes}
    default

The value 'local' starts the application locally. This is the default. The rest of the keys correspond to what is available for the type 'Distributed' in the [application:load/2 man page](http://www.erlang.org/doc/apps/kernel/application.html#load-2). The value 'default' starts the application using the values in the kernel config file. The value '{distributed, Time, Nodes}' starts the application distributed over Nodes with a restart interval of Time milliseconds. If Time is not given, the default is 0 (immediate restart). If Nodes is not given, the default is to start on the current node, then failover to any node connected _at the time of call_. (That is, gen_app just reads the current value of nodes() to supply the failover nodes.)

Upon application failover or takeover, gen\_app currently just logs that this occurs (using SASL error\_logger) then starts the application. (Q: is support for failover/takeover actions needed?)

Application key restrictions
----------

You may _not_ specify the key 'mod' among the AppKeys, since the
startup code is always generated by gen\_app. Similarly, the key
'start\_phases' is not permitted, since
the gen\_app framework does not support it. Gen\_app detects both situations and throws an error.

Supervisor specifications
-------------------------

A supervisor specification has the following types. The
server_spec() tells the system how to start the supervisor.
The sup() type describes the recursive tree of supervisors
and workers. 

    server_spec() ::=
      {local, name()}
    | {global, name()}
    | {via, module(), name()}

    sup() ::=
       {supervisor, sup_name(), sup_strategy(), [child()]}
    |  {supervisor, sup_name(), [child()]}
    |  {supervisor, sup_name()}
    |  {sup,        sup_name(), sup_strategy(), [child()]}
    |  {sup,        sup_name(), [child()]}
    |  {sup,        sup_name()}
    |  {env, [{key(), any()}], child()}

    sup_strategy() ::=
      strategy() | {strategy(), maxR(), maxT()}

Types strategy(), maxR() and maxT() are taken from the OTP 'supervisor'
module. A supervisor is specified using the 'supervisor' or 'sup' tags,
with some optional fields.

The type child() is an extension of supervisor children:

    child() ::=
      module()
    | {name(), module()}
    | sup()
    | {bridge,         {module(), function(), [any()]}}
    | {bridge, name(), {module(), function(), [any()]}}
    | {tables, name(), [tabspec()]}
    | {id(), {module(), function(), [any()]}, restart(), shutdown(), type(), modules()}
    | {id(), {module(), function(), [any()]}, restart(), shutdown()}

    modules() ::= dynamic | [module()]

Specifying module() as a child means the module is invoked and the worker
registered under the name of the module. Specifying {Name, Module} means the
module is used to start the worker, and the worker is registered as Name. A child
that is a sup() is a supervisor started by gen\_sup. Finally, a child() can be
a conventional full supervisor child specification. If the type() is omitted, it
is assumed to be 'worker'. If modules() is omitted, it is assumed to
be 'dynamic'.

Specifying a child 'bridge' spawns the supplied MFArgs 
{M, F,[Arg1,...,ArgN]} as a named or unnamed process using
supervisor\_bridge. 
Use this to statically hook ordinary processes into the supervisor tree.

Specifying a child 'tables' means a gen\_table\_owner is started, which then
loads or creates the specified tables. This is useful when you want a simple
form of persistent tables (that survive a worker crash). Note that gen\_table\_owner
can be used dynamically as well as inside a supervisor spec.

Finally, a sup() tree can contain 'env' nodes. These implement a way to configure
the tree: subterms in the tree can have the value {key, K}, which means we lookup
K in the current environment and replace it by the found value. This functionality
provides a simple way to parametrize sup() trees.

(TBD: More description of env needed + examples.)

Dynamic use of gen\_app
-----------------------

All of the modules can be invoked dynamically as well (that is, they need not be
invoked in the OTP application framework). Gen\_app will create a
new application dynamically. Gen\_sup can be started dynamically, and you can
add children dynamically too. Gen\_table\_owner can be used to start a persistent
table owner dynamically, and can be attached as a child to a gen\_sup supervisor.

Status
======

- Works in preliminary tests (i.e., the most straightforward use
cases).
- Interfaces are reasonably stable, but we may want to
change some names.
- Will remain in 0.9.* until more thoroughly tested and bugs are worked
out, whereafter we go to 1.0. Future extensions will be added in 2.0.
- Implemented for R16, not tested for other releases

Pull requests
=============

The following are known deficiencies; pull requests are
welcome.

- make sure simple_one_for_one strategy is correctly
  supported
- clean up the current code, nicer logging
- clean up the Makefile, in particular paths and use
  of smart\_exceptions
- rebar build
- automated tests
- add types and function types for dialyzer

Future extensions that are being considered

- generalize to handle a wider class of applications
  by passing funs to initialize, failover, takeover, etc.
- generalize to handle distributed apps, including
  failover, takeover
- (allow override of gen\_app 'mod' key by user)
