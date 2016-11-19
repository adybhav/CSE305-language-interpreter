
import java.io.*;
import java.util.*;

public class Interpreter{

    public static Map<String,Map<String,String>> dict_map;
    public static ArrayList<ArrayList<String>> alist_instr;
    public static ArrayList<ArrayList<String>> inout_instr;
    public static String[] test;

    static ArrayList<String> mainlist =new ArrayList<String>();

    public static void push(String j,Stack s) {
        s.push(j);
    }

    public static boolean CheckString(String str) {
        for (char c : str.toCharArray()) {
            if (!Character.isDigit(c))
                return false;
        }
        return true;
    }

    public static int dolet(Stack s,Map<String,String> dict,Map<String,Map<String,String>> mapOfdict,ArrayList<ArrayList<String>> funInstr,ArrayList<ArrayList<String>> inoutfunInstr,int ind,ArrayList<String> mainlist, int counter){
        Stack newstack = new Stack();



        Map<String,String> copy = new TreeMap<String, String>();
        Map<String,Map<String,String>>dict_map= new TreeMap<String,Map<String,String>>();
        ArrayList<ArrayList<String>> alist_instr= new ArrayList<ArrayList<String>>();
        ArrayList<ArrayList<String>> inout_instr= new ArrayList<ArrayList<String>>();
        counter=counter+1;
        String line=mainlist.get(counter);




        while(!line.startsWith("end")){

            line=mainlist.get(counter);
            System.out.print(line+"\n");

            if (line.startsWith("fun ")){
                counter=deffunction(line,newstack,copy,dict_map,alist_instr,ind,mainlist,counter);
            }
            if (line.startsWith("inOutFun ")){


                counter = definoutfunction(line,newstack,copy,dict_map,inout_instr,ind,mainlist, counter);



            }


           counter= checker(line,newstack,copy,dict_map,alist_instr,inout_instr,ind,mainlist,counter);

            if (line.startsWith("end")) {

                if (newstack.isEmpty()) {

                } else {


                    s.push(newstack.pop());



                    return counter;


                }

            }



            counter=counter+1;



        }






        return counter ;
    }


    public static int inoutfunction(Stack s,Map<String,String> dict,Map<String,Map<String,String>> mapOfdict,ArrayList<ArrayList<String>> funInstr,ArrayList<ArrayList<String>> inoutfunInstr,int ind, int counter){

        int count=0,counter1=0;
        Stack tempstack = new Stack();
        ind=0;
        int b1=0;
        int blank=0;
        String line;


        String name = s.pop().toString();
        String a= s.pop().toString();



        for(int i=0;i<inoutfunInstr.get(count).size();i++){

            if(inoutfunInstr.get(i).contains(name)){




                String arg=inoutfunInstr.get(i).get(1);

                for(int j=2;j<inoutfunInstr.get(count).size();j++){

                    if(inoutfunInstr.get(i).get(j).contains(arg))
                    { blank=1;
                        if (inoutfunInstr.get(i).get(j).contains("push ")){

                            inoutfunInstr.get(i).set(j,"push "+a);
                        }
                        ind=i;
                    }

                }
                if(count<inoutfunInstr.size()-1){
                    count++;}
                else{break;}

            }
            if (blank==1){

            }}
        line=inoutfunInstr.get(ind).get(b1);
        mapOfdict.get(inoutfunInstr.get(ind).get(0)).put(inoutfunInstr.get(ind).get(1),a);

        b1++;

        while (line!="funEnd"){

            line = inoutfunInstr.get(ind).get(b1);
            if (line.contains("return")){

                if (inoutfunInstr.get(ind).get(b1-1).contains( "push "+mapOfdict.get(name).get(inoutfunInstr.get(ind).get(1)))){
                    s.push(dict.get(tempstack.pop()));
                    return counter;
                }
                else
                {

                    s.push(tempstack.pop().toString());
                return counter;
                }
            }

            counter1 = checker(line, tempstack,dict,mapOfdict,funInstr,inoutfunInstr,ind,inoutfunInstr.get(ind),counter1+1);


            b1++;

        }
        return counter;

    }



    public static int function(Stack s,Map<String,String> dict,Map<String,Map<String,String>> mapOfdict,ArrayList<ArrayList<String>> funInstr,ArrayList<ArrayList<String>> inoutfunInstr, int ind, int counter){

        int count=0,counter1=0;
        Map<String,String> copy = new TreeMap<String, String>(dict);
        Stack tempstack = new Stack();
        ind=0;
        int b1=0;
        int blank=0;
        String line;

        String name = s.pop().toString();
        String a= s.pop().toString();

        if (dict.containsKey(a)){
            if (!mapOfdict.containsKey(a)){


            a=dict.get(a);}
        }


        if (a.startsWith(":error:")){
            s.push(a);
            s.push(name);
            s.push(":error:");
            return counter;

        }


        for(int i=0;i<funInstr.get(count).size();i++){

            if(funInstr.get(i).contains(name)){









                String arg=funInstr.get(i).get(1);

                for(int j=2;j<funInstr.get(count).size();j++){

                    if(funInstr.get(i).get(j).contains(arg))
                    { blank=1;
                        if (funInstr.get(i).get(j).contains("push ")){

                            funInstr.get(i).set(j,"push "+a);

                        }

                    }
                    ind=i;
                }

            }

            if(count<funInstr.size()-1){
                count++;}
            else{break;}

        }
        if (blank==1){

        }
        line=funInstr.get(ind).get(b1);


        b1++;

        mapOfdict.get(funInstr.get(ind).get(0)).put(funInstr.get(ind).get(1),a);


        while (line!="funEnd"){
            line = funInstr.get(ind).get(b1);


            if (line.contains("return")){

                if (funInstr.get(ind).get(b1-1).contains( "push "+funInstr.get(ind).get(1))){
                    s.push(dict.get(tempstack.pop()));
                    break;
                }
                else
                {

                s.push(tempstack.pop().toString());
                  break;
                }


            }

System.out.print(funInstr.get(ind));
            counter1 = checker(line, tempstack,mapOfdict.get(funInstr.get(ind).get(0)),mapOfdict,funInstr,inoutfunInstr,ind,funInstr.get(ind),counter1+1);



            b1++;

        }

        return counter;

    }


    public static String pop(Stack s) {

        return s.pop().toString();
    }
    public static boolean peek(Stack s) {
        String str=s.peek().toString();
        if(str.charAt(0)=='"'||str.equals(":true:")||str.equals(":unit:")||str.equals(":false:")||str.equals(":error:"))
        {return false;}
        else
            return true;

    }
    public static boolean isEmpty(Stack s) {
        return s.isEmpty();
    }


    public static boolean testTrue(String temp){
        if (temp == ":true:" ){
            return true;}
        else{return false;}
    }

    public static String testNum(boolean temp){
        if (temp == true) {
            return ":true:";}
        else{return ":false:";}
    }
    public static  Stack letstack = new Stack();
    public static int checker(String line,Stack s,Map<String,String> dict,Map<String,Map<String,String>> mapOfdict,ArrayList<ArrayList<String>> funInstr,ArrayList<ArrayList<String>> inoutfunInstr,int ind,ArrayList<String> mainlist,int counter){


        boolean temp;
        String a,b,atemp;
        int a1,b1;
        boolean a2,b2,c2;
        float af,bf;

        if (line.equals("and")){
            String c;
            if(isEmpty(s))
            {
                push(":error:",s);
            }

            else
            {
                a=pop(s);
                if (dict.containsKey(a)){
                    a = dict.get(a);}

                if(isEmpty(s))
                {   push(a,s);
                    push(":error:",s);
                }

                else{
                    b=pop(s);
                    if (dict.containsKey(b)){
                        b = dict.get(b);}
                    if ((a==":true:" || a==":false:") && (b==":true:" || b==":false:")){
                        a2 = testTrue(a);
                        b2=testTrue(b);
                        c2=a2&b2;
                        c=testNum(c2);
                        push(c,s);
                    }
                    else{
                        push(b,s);
                        push(a,s);
                        push(":error:",s);

                    }

                }

            }
        }

        else if(line.equals("call")){

            boolean funtemp=false, intemp = false;

            for (int i=0;i<funInstr.size();i++)
            {
                if(funInstr.get(i).contains(s.peek().toString())){
                    funtemp = true;
                }
            }

            for (int i=0;i<inoutfunInstr.size();i++)
            {
                if(inoutfunInstr.get(i).contains(s.peek().toString())){
                    intemp = true;
                }
            }
            if (mapOfdict.containsKey(s.peek().toString())){

                if (funtemp){


                   counter= function(s,dict,mapOfdict,funInstr,inoutfunInstr,ind,counter);}
                else if (intemp){

                    counter=inoutfunction(s,dict,mapOfdict,funInstr,inoutfunInstr,ind,counter);

                }
                else{push(":error:",s);}
            }

            else{
                push(":error:",s);
            }

        }
        if (line.equals("bind")){

            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else{
                a=pop(s);
                if(isEmpty(s)||a==":error:")
                {   push(a,s);
                    push(":error:",s);
                }
                else{
                    b=pop(s);
                    if (dict.containsKey(b)){
                        if (dict.containsKey(a)&&dict.get(a)=="empty"){
                            push(b,s);
                            push(a,s);
                            push(":error:",s);
                        }
                        else if (dict.containsKey(a)&&dict.get(a)!="empty"){
                            dict.put(b,dict.get(a));
                            push(":unit:",s);
                        }
                        else{
                            dict.put(b,a);
                            push(":unit:",s);
                        }
                    }
                    else{
                        push(b,s);
                        push(a,s);
                        push(":error:",s);
                    }
                }}}


        if (line.equals("equal")){
            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else{
                a=pop(s);
                if (dict.containsKey(a)){ a=dict.get(a);
                    if (!Character.isDigit(a.charAt(0))){    push(a,s);        push(":error:",s);                         return counter;                     }}
                if(isEmpty(s))
                {   push(a,s);
                    push(":error:",s);
                }
                else{
                    b=pop(s);
                    if (dict.containsKey(b)){ b=dict.get(b);
                        if (!Character.isDigit(b.charAt(0))){    push(b,s);        push(":error:",s);                         return counter;                     }}
                    if((CheckString(a)||CheckString(a.substring(1))) && (CheckString(b)||CheckString(b.substring(1)))){
                        if(a.charAt(0)=='-'){a1=0-Integer.parseInt(a.substring(1));} else{a1=Integer.parseInt(a);}
                        if(b.charAt(0)=='-'){b1=0-Integer.parseInt(b.substring(1));}else{b1=Integer.parseInt(b);}
                        if(a1 == b1){push(":true:",s);}else{push(":false:",s);}
                    }
                    else{push(b,s);push(a,s);push(":error:",s);}
                }}}

        if (line.equals("lessThan")){
            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else{
                a=pop(s);
                if (dict.containsKey(a)){ a=dict.get(a);
                    if (!Character.isDigit(a.charAt(0))){    push(a,s);        push(":error:",s);                         return counter;                     }}
                if(isEmpty(s))
                {   push(a,s);
                    push(":error:",s);
                }
                else{
                    b=pop(s);
                    if (dict.containsKey(b)){ b=dict.get(b);
                        if (!Character.isDigit(b.charAt(0))){    push(b,s);        push(":error:",s);                         return counter;                     }}
                    if((CheckString(a)||CheckString(a.substring(1))) && (CheckString(b)||CheckString(b.substring(1)))){
                        if(a.charAt(0)=='-'){a1=0-Integer.parseInt(a.substring(1));} else{a1=Integer.parseInt(a);}
                        if(b.charAt(0)=='-'){b1=0-Integer.parseInt(b.substring(1));}else{b1=Integer.parseInt(b);}
                        if(a1 > b1){push(":true:",s);}else{push(":false:",s);}
                    }
                    else{push(b,s);push(a,s);push(":error:",s);}
                }}}

        if (line.equals("or")){
            String c;
            if(isEmpty(s))
            {
                push(":error:",s);
            }

            else
            {
                a=pop(s);
                if (dict.containsKey(a)){
                    a = dict.get(a);}
                if(isEmpty(s))
                {   push(a,s);
                    push(":error:",s);
                }

                else{
                    b=pop(s);
                    if (dict.containsKey(b)){
                        b = dict.get(b);}
                    if ((a==":true:" || a==":false:") && (b==":true:" || b==":false:")){
                        a2 = testTrue(a);
                        b2=testTrue(b);
                        c2=a2|b2;
                        c=testNum(c2);
                        push(c,s);
                    }
                    else{
                        push(b,s);
                        push(a,s);
                        push(":error:",s);

                    }

                }

            }
        }

        if (line.equals("if")){
            String c;
            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else{
                a=pop(s);
                if(isEmpty(s))
                {   push(a,s);
                    push(":error:",s);
                }
                else{
                    b=pop(s);
                    if(isEmpty(s))
                    {   push(b,s);push(a,s);
                        push(":error:",s);
                    }
                    else{
                        c=pop(s);
                        if (dict.containsKey(c)){
                            c = dict.get(c);}
                        if(c==":true:"){push(a,s);}
                        else if (c==":false:"){push(b,s);}
                        else {push(":error:",s);}
                    }}}}


        if (line.equals("not")){
            String c;
            if(isEmpty(s))
            {
                push(":error:",s);
            }

            else
            {
                a=pop(s);
                if (dict.containsKey(a)){
                    a = dict.get(a);}
                if (a==":true:" || a==":false:"){
                    a2 = testTrue(a);
                    c2= !a2;
                    c=testNum(c2);
                    push(c,s);
                }
                else{
                    push(a,s);
                    push(":error:",s);

                }

            }

        }


        if (line.contains("push")){


            a=line.substring(5,line.length());






            if (a==":true:" || a== ":false:" || a==":unit:"){
                push(a,s);
            }
            else if(a.charAt(0)=='"'){
                atemp=a.substring(0,a.length());
                push(atemp,s);

            }
            else if(a.substring(0,1).matches("[a-z]+")||a.substring(0,1).matches("[A-Z]+")){
                if(dict.containsKey(a)){push(a,s);}
                else{
                    dict.put(a,"empty");
                    push(a,s);
                }}
            else if(a.charAt(0) == '-'){
                atemp=a.substring(1);
                if(a.charAt(1) == '0'){
                    push("0",s);}
                else{
                    if(atemp.matches("[0-9]+")){
                        bf =Float.parseFloat(atemp);
                        a1=Integer.parseInt(atemp);
                        if(bf%a1==0)
                        {push(a,s);}
                        else
                        {push(":error:",s);}} else{push(":error:",s);}}}
            else{
                if(a.charAt(0) == '0'){
                    push("0",s);}
                else{
                    if(a.matches("[0-9]+")){
                        bf =Float.parseFloat(a);
                        a1=Integer.parseInt(a);
                        if(bf%a1==0)
                        {push(a,s);}
                        else
                        {push(":error:",s);}
                    } else{push(":error:",s);}}}

        }

        if (line.equals("neg")){

            if(isEmpty(s))
            {push(":error:",s);}
            else
            {
                if(peek(s))
                { a=pop(s);
                    if (dict.containsKey(a)){
                        if (!dict.get(a).contains("empty")){

                        a=dict.get(a);}

                        if (!Character.isDigit(a.charAt(0))){    push(a,s);        push(":error:",s);                         return counter;                     }}
                    if(a.substring(0,1)=="-"){a1 =0-(Integer.parseInt(a.substring(1)));}
                    else{
                        a1=Integer.parseInt(a);}
                    push(Integer.toString(-a1),s);
                }
                else
                {push(":error:",s);}
            }}


        if (line.equals("add")){

            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else
            {
                if(peek(s))
                {
                    a=pop(s);

                    if (dict.containsKey(a)){ a=dict.get(a);
                        if (!Character.isDigit(a.charAt(0))){    push(a,s);        push(":error:",s);                         return counter;                     }}
                    if(a.substring(0,1)=="-"){a1 =0-(Integer.parseInt(a.substring(1)));}
                    else{
                        a1=Integer.parseInt(a);}
                    if(isEmpty(s))
                    {
                        push(a,s);
                        push(":error:",s);
                    }
                    else
                    {
                        if(peek(s))
                        {
                            b=pop(s);
                            if (dict.containsKey(b)){ b=dict.get(b);
                                if (!Character.isDigit(b.charAt(0))){    push(b,s);        push(":error:",s);                         return counter;                     }}
                            if(b.substring(0,1)=="-"){b1 =0-(Integer.parseInt(b.substring(1)));}
                            else{ b1=Integer.parseInt(b);}
                            push(Integer.toString(b1+a1),s);
                        }
                        else
                        {
                            push(a,s);
                            push(":error:",s);
                        }
                    }}
                else
                {
                    push(":error:",s);
                }
            }

        }


        if (line.equals("sub")){

            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else
            {
                if(peek(s))
                {
                    a=pop(s);
                    if (dict.containsKey(a)){ a=dict.get(a);
                        if (!Character.isDigit(a.charAt(0))){    push(a,s);        push(":error:",s);                         return counter;                     }}
                    if(a.substring(0,1)=="-"){a1 =0-(Integer.parseInt(a.substring(1)));}
                    else{
                        a1=Integer.parseInt(a);}
                    if(isEmpty(s))
                    {
                        push(a,s);
                        push(":error:",s);
                    }
                    else
                    {
                        if(peek(s))
                        {
                            b=pop(s);
                            if (dict.containsKey(b)){ b=dict.get(b);
                                if (!Character.isDigit(b.charAt(0))){    push(b,s);        push(":error:",s);                         return counter;                     }}
                            if(b.substring(0,1)=="-"){b1 =0-(Integer.parseInt(b.substring(1)));}
                            else{ b1=Integer.parseInt(b);}
                            push(Integer.toString(b1-a1),s);
                        }
                        else
                        {
                            push(a,s);
                            push(":error:",s);
                        }
                    }}
                else
                {
                    push(":error:",s);
                }
            }
        }

        if (line.equals("mul")){

            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else
            {
                if(peek(s))
                {
                    a=pop(s);
                    if (dict.containsKey(a)){ a=dict.get(a);
                        if (!Character.isDigit(a.charAt(0))){    push(a,s);        push(":error:",s);                         return counter;                     }}
                    if(a.substring(0,1)=="-"){a1 =0-(Integer.parseInt(a.substring(1)));}
                    else{
                        a1=Integer.parseInt(a);}
                    if(isEmpty(s))
                    {
                        push(a,s);
                        push(":error:",s);
                    }
                    else
                    {
                        if(peek(s))
                        {
                            b=pop(s);
                            if (dict.containsKey(b)){ b=dict.get(b);
                                if (!Character.isDigit(b.charAt(0))){    push(b,s);        push(":error:",s);                         return counter;                     }}
                            if(b.substring(0,1)=="-"){b1 =0-(Integer.parseInt(b.substring(1)));}
                            else{ b1=Integer.parseInt(b);}
                            push(Integer.toString(b1*a1),s);
                        }
                        else
                        {
                            push(a,s);
                            push(":error:",s);
                        }
                    }}
                else
                {
                    push(":error:",s);
                }
            }
        }


        if (line.equals("div")){

            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else
            {
                if(peek(s))
                {
                    a=pop(s);
                    if (dict.containsKey(a)){ a=dict.get(a);
                        if (!Character.isDigit(a.charAt(0))){    push(a,s);        push(":error:",s);                         return counter;                     }}
                    if(a.substring(0,1)=="-"){a1 =0-(Integer.parseInt(a.substring(1)));}
                    else{
                        a1=Integer.parseInt(a);}
                    if (a1 == 0){
                        push(a,s);
                        push(":error:",s);
                       return counter;
                    }

                    if(isEmpty(s))
                    {
                        push(a,s);
                        push(":error:",s);
                    }
                    else
                    {
                        if(peek(s))
                        {
                            b=pop(s);
                            if (dict.containsKey(b)){ b=dict.get(b);
                                if (!Character.isDigit(b.charAt(0))){   
                                	push(b,s);        push(":error:",s);                         return counter;                     }}
                            if(b.substring(0,1)=="-"){b1 =0-(Integer.parseInt(b.substring(1)));}
                            else{ b1=Integer.parseInt(b);}

                                push(Integer.toString(b1 / a1), s);

                        }
                        else
                        {
                            push(a,s);
                            push(":error:",s);
                        }
                    }}
                else
                {
                    push(":error:",s);
                }
            }
        }
        if (line.equals("let")){

            counter = dolet(s,dict,mapOfdict,funInstr,inoutfunInstr,ind,mainlist,counter);

            return counter;


        }


        if (line.equals("rem")){

            if(isEmpty(s))
            {
                push(":error:",s);
            }
            else
            {
                if(peek(s))
                {
                    a=pop(s);
                    if (dict.containsKey(a)){ a=dict.get(a);
                        if (!Character.isDigit(a.charAt(0))){    push(a,s);        push(":error:",s);                         return counter;
                        }}
                    if(a.substring(0,1)=="-"){a1 =0-(Integer.parseInt(a.substring(1)));}
                    else{
                        a1=Integer.parseInt(a);}
                    if(isEmpty(s))
                    {
                        push(a,s);
                        push(":error:",s);
                    }
                    else
                    {
                        if(peek(s))
                        {
                            b=pop(s);
                            if (dict.containsKey(b)){ b=dict.get(b);
                                if (!Character.isDigit(b.charAt(0))){    push(b,s);        push(":error:",s);                         return counter;
                                }}
                            if(b.substring(0,1)=="-"){b1 =0-(Integer.parseInt(b.substring(1)));}
                            else{ b1=Integer.parseInt(b);}
                            push(Integer.toString(b1%a1),s);
                        }
                        else
                        {
                            push(a,s);
                            push(":error:",s);
                        }
                    }}
                else
                {
                    push(":error:",s);
                }
            }
        }

                if(line.equals("swap"))
        {
            if(!isEmpty(s))
            {
                a=pop(s);
                if(!isEmpty(s))
                {
                    b=pop(s);
                    push(a,s);
                    push(b,s);
                }
                else
                {
                    push(a,s);
                    push(":error:",s);
                }
            }
            else
            {
                push(":error:",s);
            }
        }


        if (line.contains(":true:")){push(":true:",s);}

        if (line.contains(":false:")){push(":false:",s);}

        if (line.contains("pop")){
            if(isEmpty(s)){push(":error:",s);}
            else{s.pop();}
        }
 return counter;

    }

    public static int deffunction(String line,Stack s,Map<String,String> dict,Map<String,Map<String,String>> mapOfdict,ArrayList<ArrayList<String>> funInstr,int ind,ArrayList<String> mainlist,int counter){
        push(":unit:",s);

        ArrayList t1=new ArrayList();
        String p=line.substring(4);
        int aint=p.indexOf(' ');
        String fname=p.substring(0,aint);
        String arg=p.substring(aint+1);

        t1.add(fname);
        t1.add(arg);
        Map<String,String> copy = new TreeMap<String, String>(dict);
        mapOfdict.put(fname,copy);
        counter=counter+1;
        line=mainlist.get(counter);
        counter=counter+1;
        while(!line.equals("funEnd")){
            t1.add(line);
            line=mainlist.get(counter);
            counter=counter+1;
        }
        counter=counter-1;
        t1.add("funEnd");
        funInstr.add(t1);


        return counter;
    }

    public static int definoutfunction(String line,Stack s,Map<String,String> dict,Map<String,Map<String,String>> mapOfdict,ArrayList<ArrayList<String>> inoutfunInstr,int ind,ArrayList<String> mainlist,int counter){
        push(":unit:",s);
        ArrayList t1=new ArrayList();
        String p=line.substring(9);
        int aint=p.indexOf(' ');
        String fname=p.substring(0,aint);
        String arg=p.substring(aint+1);
        t1.add(fname);
        t1.add(arg);
        Map<String,String> copy = new TreeMap<String, String>(dict);
        mapOfdict.put(fname,copy);
        counter=counter+1;
        line=mainlist.get(counter);
        counter=counter+1;
        while(!line.equals("funEnd")){
            t1.add(line);
            line=mainlist.get(counter);
            counter=counter+1;
        }
        counter=counter-1;
        t1.add("funEnd");
        inoutfunInstr.add(t1);
        return counter;
    }

    public static Stack mainstack= new Stack();

    public static void hw4(String inFile,String outFile) {
        Boolean temp;
        String a,b,atemp;
        int a1,ind=0;
        boolean a2,b2,c2;
        float af,bf;
        int counter;


        Map<String,Map<String,String>> mapOfdict = new TreeMap<String,Map<String,String>>();
        ArrayList<ArrayList<String>> funInstr=new ArrayList<ArrayList<String>>();
        ArrayList<ArrayList<String>> inoutfunInstr=new ArrayList<ArrayList<String>>();

        Map<String,String> dictonary = new TreeMap<String, String>();

        String line = null;
        try {
            FileReader fileReader = new FileReader(inFile);
            FileWriter file=new FileWriter(outFile);
            BufferedWriter bw=new BufferedWriter(file);
            BufferedReader br =new BufferedReader(fileReader);
            BufferedReader br1 =new BufferedReader(fileReader);

            while ((line = br1.readLine()) != null){
                mainlist.add(line);
            }


            for ( counter=0;counter<mainlist.size();counter++)
            { line= mainlist.get(counter);

                if (line.startsWith("fun ")){
                   counter =deffunction(line,mainstack,dictonary,mapOfdict,funInstr,ind,mainlist,counter);
                }
                else if (line.startsWith("inOutFun ")){
                    counter = definoutfunction(line,mainstack,dictonary,mapOfdict,inoutfunInstr,ind,mainlist,counter);

                }

                else if (line.equals("quit"))
                {
                    while(!isEmpty(mainstack)){
                        a=pop(mainstack);
                        if(a.charAt(0) == '"'){a=a.substring(1,a.length()-1);}
                        bw.write(a);
                        bw.write("\n");
                    }
                }
                else {
                    counter = checker(line,mainstack,dictonary,mapOfdict,funInstr,inoutfunInstr,ind,mainlist,counter);
                }
            }
            bw.close();
            br.close();
            br1.close();

        }

        catch(FileNotFoundException ex) {
            System.out.println(
                    "Unable to open file '"
            );
        }
        catch(NullPointerException ex) {
            System.out.println(
                    "Unable to open file '"
            );
        }
        catch(IOException ex) {
            System.out.println(
                    "Error reading file '"
            );
        }
    }
}
