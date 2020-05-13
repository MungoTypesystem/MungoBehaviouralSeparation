class readable[{printHello; end}] {
	void printHello() {
		print("hello")
	}
}

class testable[{func; end}] {
	readable r1
	readable r2
	void func(readable[{printHello; end}] -> none x1, 
              readable[{printHello; end}] -> none x2) {
		r1 = x1;
		r1.printHello();
        r2 = x2;
        r2.printHello()
	}
} 

class testableRHS[{func; end}] {
	readable r1
	void func(void -> void x0, 
              readable[{printHello; end}] -> none x1) {
		r1 = x1;
		r1.printHello()
	}
} 

class testableLHS[{func; end}] {
	readable r1
	void func(readable[{printHello; end}] -> none x1,
              void -> void x0) {
		r1 = x1;
		r1.printHello()
	}
}

class main[{main; end}] {
	readable r2
	readable r1
	testable t
	void main() {
		t = new testable;
		r1 = new readable;
		r2 = new readable;
        t.func(r1, r2)
	}
}
