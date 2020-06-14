class File [({open; rec X.{isEmpty; <{close; end}, {read; X}>}} | {nothing; end}).end] {

    
    void open() {
        unit
    }
    
	string next
    bool isEmpty() {
        next = input();
        next == ""
    }

    string read() {
        next
    }

    void close() {
        unit
    }
	void nothing() {
		unit
	}
}



class main[{main; end}] {
    File f
    string lastLine    
    void main() {
        f = new File;
        f.open();
        (loop: 
            if (f.isEmpty()) {
                f.close()
            } else {
                lastLine = f.read();
                print(lastLine);
                continue loop
            }
        );
		f.nothing()
    }
}