class File [{open; rec X.{isEmpty; <{close; end}, {read; X}>}}] {

    string next
    void open() {
        unit
    }
    
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
        )
    }
}