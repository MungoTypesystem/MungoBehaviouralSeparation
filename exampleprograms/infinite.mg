class Infinite [ rec X. {shouldContinue; <X, end> }] {
    bool shouldContinue() {
        true
    }
}

class main [{main; end}] {
    Infinite i
    int c
    void main() {
        i = new Infinite;
        (loop: 
            if (i.shouldContinue()) {
                c = (c + 1);
                print(c);
                continue loop
            } else {
                unit
            })
    }
}
