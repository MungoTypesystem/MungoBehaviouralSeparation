class PriceValidator[{isFairPrice; <end, end>}] {
    bool isFairPrice(int x) {
        (x <= 100)
    }
}

class Customer[{bargain; {finalize; end}}] {
    int price
    PriceValidator pv
    Service s
    int date

    void bargain(Agency[{getQuote; rec X.{getQuote; X accept; end}}] -> Agency[end] agent) {
        pv = new PriceValidator;
        price = agent.getQuote();
        if (pv.isFairPrice(price)) {
            print("Initial price accepted");
            print(price);
            s = agent.accept()
        } else {
            pv = new PriceValidator;
            loop: (
                price = agent.getQuote();
                print("New price received from agency");
                print(price);
                if (pv.isFairPrice(price)) {
                    print("Accepted price");
                    print(price);
                    s = agent.accept()
                } else {
                    pv = new PriceValidator;
                    continue loop
                }
            )
        }
        
    }

    void finalize() {
        s.setAddress("Selma Lagerlöfs Vej 300");
        date = s.getDate()
    }
}

class Agency[{init; {getQuote; rec X.{getQuote; X accept; end}}}] {
    int curQuote

    void init() {
        curQuote = (210)
    }

    int getQuote() {
        curQuote = (curQuote - 10);
        curQuote
    }

    Service[{setAddress; {getDate; end}}] accept() {
        new Service
    }

}

class Service[{setAddress; {getDate; end}}] {
    string address
    void setAddress(string x) {
        address = x
    }

    int getDate() {27}
}

class main[{main; end}] {
    Customer c
    Agency a
    
    void main() {
        c = new Customer;
        a = new Agency;
        a.init();
        c.bargain(a);
        c.finalize()
    }
}