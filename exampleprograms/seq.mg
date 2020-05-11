class Printer [rec X.{print; X finish; end}] {
    int balance
    void print(Account[{getBalance; end} -> Account[end] x]) {
        balance = x.getBalance();
        print(balance)
    }
}

class Account [
        {getBalance; end};{addSalary; {applyInterest; end}};{getBalance; end}
    ] {

    int balance

    int getBalance() {
        balance
    }

    void addSalary() {
        balance = balance + 16000
    }

    void applyInterest() {
        balance = balance * 1.03
    }
}

class main [{main; end}] {
    Account acc
    Printer p

    void main() {
        acc = new Account;
        p = new Printer;
        p.print(acc);
        acc.addSalary();
        acc.applyInterest();
        p.print(acc);
        p.finish()
    }
}