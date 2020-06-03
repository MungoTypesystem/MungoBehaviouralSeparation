class Pair[({setLeft; end} | {setRight; end}).{sum; end}] {
    int left
    int right

    void setLeft(int x) { left = x }
    
    void setRight(int x) { right = x }

    int sum() { left + right }
}

class PairUser[{initialise; end}] {
    void initialise(Pair[{setLeft; end}] -> Pair[end] l, 
                    Pair[{setRight; end}] -> Pair[end] r) {
        l.setLeft(2);
        r.setRight(5)
    }
}

class main[{main; end}] {
    Pair p
    PairUser pu
    int res

    void main() {
        p = new Pair;
        pu = new PairUser;
        pu.initialise(p, p);
        res = p.sum();
        print(res)
    }
}