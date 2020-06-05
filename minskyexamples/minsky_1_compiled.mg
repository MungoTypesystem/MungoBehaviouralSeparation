class main[{main; end}] {Inst machine void main() { machine = new Inst; machine.i1(0, 0) }}
class BoolValidator[{validate;<end, end>}] { bool validate(bool b) { b } }
class Inst[{i1; end  i2; end  i3; end  i4; end }] { Inst i bool b BoolValidator v int r1 int r2 
void i1(int x1, int x2) { r1 = x1; r2 = x2; i = new Inst; r1 = (r1 + 1);i.i2(r1, r2); i = null}
void i2(int x1, int x2) { r1 = x1; r2 = x2; i = new Inst; b = (r2 == 0); v = new BoolValidator; if (v.validate(b)) {i.i4(r1, r2)} else { r2 = (r2 - 1); i.i3(r1, r2) }; v = null; i = null}
void i3(int x1, int x2) { r1 = x1; r2 = x2; i = new Inst; r1 = (r1 + 1);i.i4(r1, r2); i = null}
void i4(int x1, int x2) { r1 = x1; r2 = x2; print(r1);print(r2); i = null}}