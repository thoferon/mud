Mud is a small tool to automate deployment and undeployment of applications. It
stands for Multi Version Deployer as it has been built for this purpose.

# Configuration

The simpler case is an application having one deploy script and one undeploy
script. The configuration files are in `/usr/local/etc/mud` by default (it also
depends on your system.) For instance, if the application you want to deploy is
called 'myApp', mud will read the file `/usr/local/etc/mud/myApp.conf`.

IMPORTANT NOTE: This directory should NOT be world-writable as an attacker could
use it to add other files like common typos (myapp.conf, myAPp.conf, etc.) which
would be read and "executed" by the user calling mud should he/she misspelled
the project name. Of course, it would be even worse if the files themselves are
world-writable.

A configuration file looks like this:

    deploy=/usr/local/etc/mud/myApp.deploy
    undeploy=/usr/local/etc/mud/myApp.undeploy
    basepath=/tmp
    user=www
    group=www
    version=1b348a723f19f241c23e1
    var:customVar=customValue

Do not put spaces around `=`. Also all of these variables are optional.

By default, mud does not change user or group if not specified, passes `/tmp` as
basepath to the (un)deploy, has an empty string as version and no custom
 variables. The default paths to the scripts are
`SYSCONFDIR/mud/PROJECTNAME.deploy` and `SYSCONFDIR/mud/PROJECTNAME.undeploy`.

## Multiple configurations

It's also possible to have several configuration files for one project.
For example, you might want to run different steps with different users. In
order to do that, create a directory with the same path than if you had one
configuration file but without the extension. In this example, it should be
`/usr/local/etc/mud/myApp`. Mud will read all the files finishing with `.conf`
in the directory and "run" them in alphabetical order.

Note that if a `.conf` file exists, the directory will be ignored.

## Custom variables

Custom variables specified with `var:myName=myValue` are set as environment
variables when running the scripts. The last occurence of a variable assignment
wins. In the following example, `EDITOR` will be set to `emacs`.

    var:EDITOR=vim
    var:EDITOR=emacs

# Options and arguments

You can force the use of a particular user or group with `-u` / `--user` and
`-g` / `--group`. To run the undeploy script, use the `--undeploy` option.
Finally, if you just want to see what commands will be executed, use the
`--dry-run` option.

The short version of the arguments is as follow:

    mud project_name [version [basepath]] [-- custom_args]

`version` and `basepath` overwrites their equivalent in the configuration. The
custom arguments are simply passed to the script(s). For instance, you could
type something like this:

    mud website v1.0 /var/www -- --verbose

# (Un)deploy scripts

The scripts are passed the project name, the version, the base path, any custom
arguments from the command-line and finally, any custom variables from the
configuration as `myName=myValue`.

They should return an exit code of 0 in case of success and anything else
otherwise.

# Production and security

Mud is aimed to be used by root as well as unprivileged users to deploy
applications: you might want your build system to be able to deploy the program
without having access to this part of the system though. In order to achieve
this, you can give sudo access to your CI user (say you use Court and the user
is `_court`) and another user `j.smith` with a configuration like the folowing
in `/etc/sudoers`. (Use `visudo`!)

    User_Alias DEPLOYERS = _court, j.smith
    Host_Alias WEBSERVERS = www.website.com, staging.website.com

    DEPLOYERS WEBSERVERS = NOPASSWD: /usr/local/bin/court

**IMPORTANT:** Mud prevents the user to choose a configuration file outside of
the configuraton directory (something like /usr/local/etc/mud depending on your
system). You need to make sure that this directory and its contents is NOT
world-writable.
