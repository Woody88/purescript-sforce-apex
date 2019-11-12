class MyClass{
    /* https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/langCon_apex_loops.htm */
    // public void ifElse(){
    //     Integer x, sign;
    //     // Your code
    //     if (x <= 0) if (x == 0) sign = 0; else sign = -1;
    // }

    // public void repeatedElseIf(){
    //     if (place == 1) {
    //         medal_color = 'gold';
    //     } else if (place == 2) {
    //         medal_color = 'silver';
    //     } else if (place == 3) {
    //         medal_color = 'bronze';
    //     } else {
    //         medal_color = null;
    //     }
    // }

    public void switchFunc(){
        switch on i { 
            when 2 { 
                System.debug('when block 2'); 
            } 
            when -3 { 
                System.debug('when block -3'); 
            } 
            when else { 
                System.debug('default'); 
            } 
        }
    }

    // public void doWhileLoop(){
    //     Integer count = 1;

    //     do {
    //         System.debug(count);
    //         count++;
    //     } while (count < 11);
    // }

    // public void whileLoop(){
    //     Integer count = 1;

    //     while (count < 11) {
    //         System.debug(count);
    //         count++;
    //     }
    // }

    // public void whileLoop(){
    //     Integer count = 1;

    //     while (count < 11) {
    //         System.debug(count);
    //         count++;
    //     }
    // }

    // public void traditionalLoop(){
    //     for (Integer i = 0, j = 0; i < 10; i++) { System.debug(i+1); }
    // }

    // public void iterationForLoop(){
    //     Integer[] myInts = new Integer[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    //     for (Integer i : myInts) {
    //         System.debug(i);
    //     }
    // }

}
