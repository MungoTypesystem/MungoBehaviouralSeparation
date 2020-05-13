
class Boolean[
        { 
            setup; (rec x. {val;end T;x}).end
            setup; (rec y. {val;end F;y}).end 
        }
	] {

	bool v

	void T() {
		v = true
	}

	void F() {
		v = false
	}

    void setup() {
        unit
    }

	bool val() {
        v
	}
}

class main[{main; end}] {
    Boolean x1
	Boolean x2
	Boolean x3
	Boolean x4
	Boolean x5
	Boolean x6
	Boolean x7
	Boolean x8
	Boolean x9
    bool b 
    void main() {
        x1 = new Boolean;
		x2 = new Boolean;
		x3 = new Boolean;
		x4 = new Boolean;
		x5 = new Boolean;
		x6 = new Boolean;
		x7 = new Boolean;
		x8 = new Boolean;
		x9 = new Boolean;
        x1.setup();
		x2.setup();
		x3.setup();
		x4.setup();
		x5.setup();
		x6.setup();
		x7.setup();
		x8.setup();
		x9.setup();
        x1.F();
		x2.F();
		x3.F();
		x4.F();
		x5.F();
		x6.F();
		x7.F();
		x8.F();
		x9.F();
        b = x1.val();
		x2.val();
		x3.val();
		x4.val();
		x5.val();
		x6.val();
		x7.val();
		x8.val();
		x9.val();
        print(b)
    }
} 
