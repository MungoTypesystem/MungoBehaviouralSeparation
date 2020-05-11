
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
    /*Boolean x2
    Boolean x3
    Boolean x4
    Boolean x5
    Boolean x6
    Boolean x7
    Boolean x8
    Boolean x9 */
    bool b 
    void main() {
        x1 = new Boolean;
        x1.setup();
        x1.T();
        x1.val();
        unit
    }
}
