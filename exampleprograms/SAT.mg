
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
    bool b 
    void main() {
        x1 = new Boolean;
        x1.setup();
        x1.T();
        b = x1.val();
        print(b)
    }
} 
