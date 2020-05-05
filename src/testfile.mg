
// testing expression 
class testReturnParameter {
    {m; end} 
    void m(void -> void x, void -> void y) {
        x
    }
}
/*
// 
class testAssignFieldAndCall {
    {m; end} 

    testReturnParameter f    

    void m(void -> void x, void -> void y) {
        f = new testReturnParameter;
        f.m(x,y)
    }
}

// 
class testCallParam {
    {m; end} 

    testReturnParameter f    

    void m(testReturnParameter[{m; end}] -> testReturnParameter[end] x, void -> void y) {
        x.m(unit, unit)
    }
} 

// 
class testCallParam2 {
    {m; end} 

    testReturnParameter f    

    void m(testReturnParameter[{m; end}] -> testReturnParameter[end] x, void -> void y) {
        x.m(y, y)
    }
}

// 
class testSeq {
    {m; end}
    void m(void -> void x, void -> void y) {
        unit; unit
    }
}

class testSeqCall {
    {m; end}
    void m(testReturnParameter[{m; end}] -> testReturnParameter[end] x, testReturnParameter[{m; end}] -> testReturnParameter[end] y) {
        x.m(unit, unit); y.m(unit, unit) 
    }
}

class testBranch {
    {m; {n; end}}

    void m(void -> void x, void -> void y) {
        unit
    }
    void n(void -> void x, void -> void y) {
        unit
    }
}

class testSeqCall {
    {m; end}
    void m(testBranch[{m; {n; end}}] -> testBranch[end] x, void -> void y) {
        x.m(unit, unit); x.n(unit, unit) 
    }
}

class testBranch2 {
    {m; end n; end}

    void m(void -> void x, void -> void y) {
        unit
    }
    void n(void -> void x, void -> void y) {
        unit
    }
}

class testBoolField {
    {m; end}
    bool b
    void m(bool -> bool x, void -> void y) {
        b = x
    }
}

class testRec {
    rec X. {m; X m; end}

    void m(void -> void x, void -> void y) {
        unit
    }
}

class testTrue {
    {m; <end, end>}
    bool m(void -> void x, void -> void y) {
        true 
    }
}

class testIf {
    {m; end}
    
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

class testTrue2 {
    {m; <{t; end}, {f; end}>}
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

class testIf2 {
    {m; end}
    
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

class testParallel1 {
    ({h; end} | {g;end}).{f;end}
    void h(void -> void x, void -> void y) {
        unit
    }

    void g(void -> void x, void -> void y) {
        unit
    }
    void f(void -> void x, void -> void y) {
        unit
    }
} */

class testParallel2 {
    ({h; end} | {g;end}).{f;end}
    
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

class testUsingParallel {
    {m; end}
    
    void m(testParallel2[{h; end}] -> testParallel2[end] x, testParallel2[{g; end}] -> testParallel2[end] y) {
        x.h(unit, unit);
        y.g(unit, unit)
    }
}

class testSplit {
    {m; end}

    testParallel2 f1
    testUsingParallel f2
    void m(void -> void x, void -> void y) {
        f1 = new testParallel2;
        f2 = new testUsingParallel;
        f2.m(f1, f1);
        f1.f(unit, unit)
    }
}

