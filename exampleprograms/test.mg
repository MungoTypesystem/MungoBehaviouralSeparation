class True [{t; <end, end>}] {
    bool t() {
        true
    }
}

class main [{main; end}] {
    True t
    void main() {
        t = new True;
        (loop: 
            if (t.t()) {
                t = new True;
                continue loop
            } else {
                unit
            });
        unit
    }
}
