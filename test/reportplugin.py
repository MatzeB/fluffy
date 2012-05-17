# New testclass
class TestFluffy(Test):
	def __init__(self, filename, environment):
		Test.__init__(self, filename, environment)
	def _init_flags(self):
		Test._init_flags(self)
		environment = self.environment
		environment.cflags = "-O3"
		environment.ldflags = ""
	def compile(self):
		environment = self.environment
		cmd = "%(compiler)s %(cflags)s %(filename)s %(ldflags)s -o %(executable)s" % environment.__dict__
		self.compile_command = cmd
		self.compiling = ""
		try:
			self.compile_out, self.compile_err, self.compile_retcode = my_execute(cmd, timeout=60)
		except SigKill, e:
			self.error_msg = "compiler: %s" % e.name
			return False
		c = self.parse_compiler_output()
		if not c: return c
		return True
	def check_compiler_errors(self):
		if self.compile_retcode != 0:
			self.error_msg = "compilation not ok (returncode %d)" % self.compile_retcode
			self.long_error_msg = "\n".join((self.compile_command, self.compiling))
			return False
		return True

class FluffyShouldFail(TestFluffy):
	def __init__(self, filename, environment):
		TestFluffy.__init__(self, filename, environment)

	def check_compiler_errors(self):
		if len(self.errors) == 0:
			self.error_msg = "compiler missed error"
			return False
		return True
	def check_execution(self):
		return True # nothing to execute

test_factories += [
	( lambda name: name.endswith(".fluffy") and "should_fail" in name, FluffyShouldFail ),
	( lambda name: name.endswith(".fluffy"), TestFluffy ),
]
_EXTENSIONS.append("fluffy")

# Configurations
def setup_fluffy(option, opt_str, value, parser):
	global _ARCH_DIRS
	_ARCH_DIRS = []
	global _DEFAULT_DIRS
	_DEFAULT_DIRS = [ "fluffy", "fluffy/should_fail" ]
	config = parser.values
	config.compiler = "fluffy"
	config.expect_url = "fluffy/fail_expectations"

configurations["fluffy"] = setup_fluffy
