type
	TTarget = (x86_64, powerpc, aarch64, riscv);

	TTest = record
		name: String;
		source: String;
		ignoredTargets: array of TTarget;
	end;

	TTestSet = array of TTest;

const
	tests: TTestSet = [
		(name: 'basic'; source: 'tests/test.pas'; ignoredTargets: [powerpc])
	];
