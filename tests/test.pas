program test;

begin
	WriteLn('Hello from test');
	Randomize;
	if Random(101) <= 50 then Halt(0);
	Halt(1);
end.
