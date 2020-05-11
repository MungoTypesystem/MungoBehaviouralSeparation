class Inifinite [ rec X. {shouldContinue; <X, end> }] {
    bool shouldContinue() {
        true
    }
}

class main [{main; end}] {
    Inifinite i
    int c
    void main() {
        i = new Inifinite;
        loop: 
            if (i.shouldContinue()) {
                c = c + 1;
                print(c);
                continue loop
            } else {
                unit
            }
    }
}
