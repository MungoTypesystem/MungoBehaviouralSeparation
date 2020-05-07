
// testing expression 
class testReturnParameter[{m; end}] {
    void m(void x) {
        x
    }
}

// 
class testAssignFieldAndCall[{m; end}]{
     

    testReturnParameter f    

    void m(void -> void x, void -> void y) {
        f = new testReturnParameter;
        f.m(x,y)
    }
}

// 
class testCallParam[{m; end}] {
     

    testReturnParameter f    

    void m(testReturnParameter[{m; end}] -> testReturnParameter[end] x, void -> void y) {
        x.m(unit, unit)
    }
} 

// 
class testCallParam2[{m; end}] {
    testReturnParameter f    

    void m(testReturnParameter[{m; end}] -> testReturnParameter[end] x, void -> void y) {
        x.m(y, y)
    }
}

// 
class testSeq[{m; end}] {
    
    void m(void -> void x, void -> void y) {
        unit; unit
    }
}

class testSeqCall[{m; end}]{
    void m(testReturnParameter[{m; end}] -> testReturnParameter[end] x, testReturnParameter[{m; end}] -> testReturnParameter[end] y) {
        x.m(unit, unit); y.m(unit, unit) 
    }
}

class testBranch[{m; {n; end}}] {
    void m(void -> void x, void -> void y) {
        unit
    }
    void n(void -> void x, void -> void y) {
        unit
    }
}

class testSeqCall[{m; end}]{
    
    void m(testBranch[{m; {n; end}}] -> testBranch[end] x, void -> void y) {
        x.m(unit, unit); x.n(unit, unit) 
    }
}

class testBranch2[{m; end n; end}] {
    void m(void -> void x, void -> void y) {
        unit
    }
    void n(void -> void x, void -> void y) {
        unit
    }
}

class testBoolField[{m; end}] {
    
    bool b
    void m(bool -> bool x, void -> void y) {
        b = x
    }
}

class testRec[rec X. {m; X m; end}] {
    void m(void -> void x, void -> void y) {
        unit
    }
}

class testTrue[{m; <end, end>}] {
    bool m(void -> void x, void -> void y) {
        true 
    }
}

class testIf[{m; end}] {
    testTrue f    

    bool m(void -> void x, void -> void y) {
        f = new testTrue;
        if (f.m(unit, unit)) {
            true
        } else {
            false
        }
    }
} 

class testTrue2[{m; <{t; end}, {f; end}>}] {
    
    bool m(void -> void x, void -> void y) {
        true 
    }
    void f(void -> void x, void -> void y) {
        unit 
    }
    void t(void -> void x, void -> void y) {
        unit 
    }
}

class testIf2[{m; end}] {
    testTrue2 f    

    void m(void -> void x, void -> void y) {
        f = new testTrue2;
        if (f.m(unit, unit)) {
            f.t(unit, unit)
        } else {
            f.f(unit, unit)
        }
    }
} 

class testParallel1[({h; end} | {g;end}).{f;end}]{
    void h(void -> void x, void -> void y) {
        unit
    }

    void g(void -> void x, void -> void y) {
        unit
    }
    void f(void -> void x, void -> void y) {
        unit
    }
} 

class testSplit[{m; end}]{

    testParallel2 f1
    testUsingParallel f2
    void m(void -> void x, void -> void y) {
        f1 = new testParallel2;
        f2 = new testUsingParallel;
        f2.m(f1, f1);
        f1.f(unit, unit)
    }
}

class testParallel2[({h; end} | {g;end}).{f;end}] {
    testReturnParameter l
    testReturnParameter r

    void h(void -> void x, void -> void y) {
        l = new testReturnParameter
    }

    void g(void -> void x, void -> void y) {
        r = new testReturnParameter
    }
    void f(void -> void x, void -> void y) {
        l.m(unit, unit);
        r.m(unit, unit)
    }
} 

class testUsingParallel[{m; end}] {
    void m(testParallel2[{h; end}] -> testParallel2[end] x, testParallel2[{g; end}] -> testParallel2[end] y) {
        x.h(unit, unit);
        y.g(unit, unit)
    }
}



class File[{open; rec X.{isEmpty; <{close; end}, {readChar; X}>}}]{

    bool isEmpty(void->void x, void->void x2) {
        true
    }

    bool readChar(void->void x, void->void x2) {false}
    void close(void->void x, void->void x2) {unit}
    void open(void->void x, void->void x2) {unit}
}

class FileReader[{readFile; end}] {
    File f

    void readFile(void->void x, void->void x2) {
        f = new File;
        f.open(unit, unit);
        loop: if (f.isEmpty(unit, unit)) {
                f.close(unit, unit)
              } else {
                 f.readChar(unit, unit);
                 continue loop
              }
    }
}



class testParallel3[({h; end} | {g;end}).{f;end}] {
    testReturnParameter l
    testReturnParameter r

    void h(void -> void x, void -> void y) {
        l = new testReturnParameter
    }

    void g(void -> void x, void -> void y) {
        r = new testReturnParameter
    }
    void f(void -> void x, void -> void y) {
        l.m(unit, unit);
        r.m(unit, unit)
    }
} 

class testSeqCallsOnParallel[{setup; {m; end n; end}}]{
    testParallel3 f

    
    void setup(void -> void x1, void -> void x2) {
        f = new testParallel3
    }

    void m(void -> void x1, void -> void x2) {
        f.h(x1, x2);
        f.g(x1, x2);
        f.f(x1, x2)
    }

    void n(void -> void x1, void -> void x2) {
        f.g(x1, x2);
        f.h(x1, x2);
        f.f(x1, x2)
    }
} 

class testParallel4[(({h1; end} | {g1;end}).end | ({h2; end} | {g2;end}).end).{f;end}]{
    
    testReturnParameter l1
    testReturnParameter l2
    testReturnParameter r1
    testReturnParameter r2

    void h1(void -> void x, void -> void y) {
        l1 = new testReturnParameter
    }

    void h2(void -> void x, void -> void y) {
        l2 = new testReturnParameter
    }

    void g1(void -> void x, void -> void y) {
        r1 = new testReturnParameter
    }

    void g2(void -> void x, void -> void y) {
        r2 = new testReturnParameter
    }

    void f(void -> void x, void -> void y) {
        l1.m(unit, unit);
        l2.m(unit, unit);
        r1.m(unit, unit);
        r2.m(unit, unit)
    }
} 

class testSeqCallsOnParallel[{setup; {m; end n;end}}] {
    

    testParallel4 f

    
    void setup() {
        f = new testParallel4
    }

    void m(void x1, void x2) {
        f.h2();
        f.h1();
        f.g2();
        f.g1();
        f.f()
    }

    void n() {
        f.g1();
        f.g2();
        f.h1();
        f.h2();
        f.f()
    }
} 

class testWeirdUsage[({m; end} | {m; end} | {m; end} | {m; end} | {m; end}).{m; end}] {
    void m() {
        unit
    }
}

class testWeirdUsage2[{m; end} ; {m; end}] {
    void m() {
        unit
    }
}

class main[{main; end}] {

    testSeqCallsOnParallel f

    void main() {
        f = new testSeqCallsOnParallel;
        f.setup();
        f.m();
        print(true);
        unit
    }
}




/*
class testBinary[{m; end}] {
    bool f
    void m() {
        f = (false == true == true != false && true || false);
        print(f); 
        unit
    }
}

class main[{main; end}] {
    testBinary t
    void main() {
        t = new testBinary;
        t.m(); 
        unit
    }
}
*/