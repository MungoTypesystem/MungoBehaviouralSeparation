class A[({m;end} | {n;end} | {o;end} | {p;end} | {q;end} | {r;end}).end] {
	void m() {unit}
	void n() {unit}
	void o() {unit}
	void p() {unit}
	void q() {unit}
	void r() {unit}
}

class B[{m; end}] {
	A f
	void m() {
		f = new A;
		f.r(); 
		f.q();
		f.p(); 
		f.o();
		f.n();
		f.m()
	}

}