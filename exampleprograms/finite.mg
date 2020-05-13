class Infinite [ rec X. {shouldContinue; <X, end> }] {
    bool shouldContinue(bool b) {
        b
    }
}

class main [{main; end}] {
    Infinite i
    int c
	bool con
    void main() {
        i = new Infinite;
        (loop: 
			con = (c != 1000000); 
            if (i.shouldContinue(con)) {
                c = (c + 1);
                continue loop
            } else {
                unit
            });
		print(c)
    }
}
