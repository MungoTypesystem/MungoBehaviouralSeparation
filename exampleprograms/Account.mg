class Account [
        {getBalance; end};{addSalary; {applyInterest; end}};{getBalance; end}
    ] {

    int balance

    int getBalance() { balance }
    void addSalary() { balance = (balance + 16000) }
    void applyInterest() { balance = (balance + 30) }
}

class Printer [rec X.{output; X finish; end}] {
    int balance
    void output(Account[{getBalance; end}] -> Account[end] x) {
        balance = x.getBalance();
        print(balance)
    }
    void finish() { unit }
}

class main [{main; end}] {
    Account acc
    Printer p

    void main() {
        acc = new Account;
        p = new Printer;
        p.output(acc);
        acc.addSalary();
        acc.applyInterest();
        p.output(acc);
        p.finish()
    }
}