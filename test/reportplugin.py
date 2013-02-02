from test.test   import Test, ensure_dir
from test.steps  import execute, step_execute
from test.checks import check_no_errors, check_firm_problems, check_retcode_zero, create_check_reference_output, check_missing_errors
import os

def step_compile_fluffy(environment):
    cmd = "%(flc)s %(filename)s %(flflags)s -o %(executable)s" % environment.__dict__
    return execute(environment, cmd, timeout=30)

def make_fluffy_test(environment, filename):
    environment.filename = filename
    environment.executable = environment.builddir + "/" + environment.filename + ".exe"
    ensure_dir(os.path.dirname(environment.executable))

    test = Test(environment, filename)

    compile = test.add_step("compile", step_compile_fluffy)
    compile.add_check(check_no_errors)
    compile.add_check(check_firm_problems)
    compile.add_check(check_retcode_zero)

    execute = test.add_step("execute", step_execute)
    execute.add_check(check_retcode_zero)
    execute.add_check(create_check_reference_output(environment))
    return test

def make_fluffy_should_fail(environment, filename):
    environment.filename = filename
    environment.executable = environment.builddir + "/" + environment.filename + ".exe"
    ensure_dir(os.path.dirname(environment.executable))

    test = Test(environment, filename)

    compile = test.add_step("compile", step_compile_fluffy)
    compile.add_check(check_missing_errors)
    return test

test_factories = [
    (lambda name: name.endswith(".fluffy") and "fluffy/should_fail/" in name, make_fluffy_should_fail),
]
wildcard_factories = [
    ("*.fluffy", make_fluffy_test),
]

# Configurations
def config_fluffy(option, opt_str, value, parser):
    config = parser.values
    config.arch_dirs    = []
    config.default_dirs = [ "fluffy", "fluffy/should_fail" ]

configurations = {
    "fluffy": config_fluffy,
}

def register_options(opts):
    opts.add_option("--flc", dest="flc",
                    help="Use FKC to compiler fluffy programs",
                    metavar="FLC")
    opts.add_option("--flflags", dest="flflags",
                    help="Use FLFLAGS to compiler fluffy programs",
                    metavar="FLFLAGS")
    opts.set_defaults(
        flc="fluffy",
        flflags="-O3",
    )
