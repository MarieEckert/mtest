type
	TTarget = (x86_64, powerpc, aarch64, riscv);

	{ DO NOT MODIFY THIS RECORD }
	TTest = record
		name: String;
		source: String;
		compileArgs: TStringDynArray;
		run: Boolean;
		args: TStringDynArray;
		ignoredTargets: set of TTarget;
	end;

	TTestSet = array of TTest;

const
	FPC_BIN = '/usr/bin/fpc';

	tests: TTestSet = [
		(
			name: 'basic';
			source: 'tests/test.pas';
			compileArgs: [];
			run: True;
			args: [];
			ignoredTargets: [powerpc]
		)
	];
