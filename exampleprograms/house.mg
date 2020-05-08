class LightController[
        { on; rec Y.{changeIntensity; Y off; end} }
    ] {
    void on() {
        print("LightController turned on")
    }
    void off() {
        print("LightController turned off")
    }
    void changeIntensity() {
        print("LightController set to high intensity")
    }
}

class TempController [
        rec A.{
            setTemperature; A
            off; end
        }
    ] {
    void off() {
        print("TempController turned off")
    }
    void setTemperature(int temp) {
        print("TempController set to high intensity");
        print(temp)
    }
}

class DoorController [
        rec B.{
            lock; B
            unlock; B
            off; end
        }
    ] {
    void off() {
        print("DoorController turned off")
    }
    
    void lock() {
        print("DoorController locked doors")
    }

    void unlock() {
        print("DoorController unlocked doors")
    }
}

class HouseController [
    (
    { initLightController; 
        {lightOn; rec C.{ 
            adjustLight; C 
            lightOff; end}}}
    |
    { initTempController; 
        rec D. {
            setTemperature; D
            turnOff; end}}
    |
    { initDoorController; 
        rec E. {
            lockDoors; E
            unlockDoors; E
            turnOff; end }}
    ).end
    ] {
    LightController lc
    TempController tc
    DoorController dc

    void initLightController() {
        lc = new LightController
    }

    void initTempController() {
        tc = new TempController
    }

    void initDoorController() {
        dc = new DoorController
    }

    void lightOn() {
        lc.on()
    }

    void adjustLight() {
        lc.changeIntensity()
    }

    void lightOff() {
        lc.off()
    }

    void setTemperature(int x) {
        tc.set(x)
    }

    void turnOff() {
        tc.off()
    }

    void lockDoors() {
        dc.lock()
    }

    void unlockDoors() {
        dc.unlock()
    }

    void turnOff() {
        dc.off()
    }
}