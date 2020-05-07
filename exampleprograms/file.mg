class File [{open; rec X.{isEmpty; <{close; end}, {read; X}>}}] {

    string next
    void open() {
        print("opened")
    }
    
    bool isEmpty() {
        print("reading > ");
        next = input();
        next == ""
    }

    string read() {
        next
    }

    void close() {
        print("Closing")
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
                print("Read");
                print(lastLine);
                continue loop
            }
        )
    }
}