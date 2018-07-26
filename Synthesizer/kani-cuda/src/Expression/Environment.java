package Expression;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

public class Environment {
	HashMap<String, Integer> head;
	HashMap<String, Integer> env;
	int N = 4;

	public Environment(){
		this.head = new HashMap<String, Integer>();
		this.env = new HashMap<String, Integer>();
	}
	
	public Environment(List<String> vars){
		int size = vars.size();
		this.head = new HashMap<String, Integer>();
		for(String name : vars.subList(0, 4)){
			this.head.put(name, 0);
		}
		this.env = new HashMap<String, Integer>();
		for(String name : vars.subList(4, size)){
			this.env.put(name, 0);
		}
	}
	
	public Environment(List<String> head, List<String> env){
		this.head = new HashMap<String, Integer>();
		for(String name : head){
			this.head.put(name, 0);
		}
		this.env = new HashMap<String, Integer>();
		for(String name : env){
			this.env.put(name, 0);
		}
	}
	
	public void putHead(String var, int val){
		this.head.put(var, val);
	}
	
	public void putEnv(String var, int val){
		this.env.put(var, val);
	}
	
	public boolean exsitsSmIdx(){
		return head.get("smix") != -1;
	}
	
	public Integer getSmIdx(){
		return head.get("smix");
	}
	
	public void setVal(List<String> vars, List<String> vals){
		int size = vars.size();
		for(int i = 0; i < N; i++){
			if (vals.get(i).equals("N")) {
				this.putHead(vars.get(i), -1);
			} else {
				this.putHead(vars.get(i), Integer.parseInt(vals.get(i)));
			}
		}
		for(int i = N; i < size; i++){
			this.putEnv(vars.get(i), Integer.parseInt(vals.get(i)));
		}
	}
	
	public List<String> getVarNames(){
		return new ArrayList<String>(this.env.keySet());
	}
	
	public List<Expression> getVars(){
		List<String> vars = this.getVarNames();
		Iterator<String> it = vars.iterator();
		List<Expression> lst = new ArrayList<Expression>();
		while(it.hasNext()){
			Expression exp1 = new Variable(it.next());
			lst.add(exp1);
			}
		return lst;
	}
	
	public int getVal(Variable var){
		return this.env.get(var.getName());
	}
	
	public List<Expression> generateArith(int depth){
		List<Expression> vars = this.getVars();
		Expression one = new Constant<Integer>(1);
		vars.add(one);
		if (depth == 0) {
			return vars;
		} else {
			List<Expression> lst1 = this.generateArith(depth-1);
			List<Expression> lst2 = this.generateArith(depth-1);
			List<Expression> res = vars;
			int size1 = lst1.size();
			int size2 = lst2.size();
			for (int i = 0; i < size1; i++) {
				for (int j = 0; j < size2; j++) {
					Expression exp1 = lst1.get(i);
					Expression exp2 = lst2.get(j);
					if (!(exp1.toStringExp().equals(exp2.toStringExp())) && !exp1.equals(one)) {
						//exp1.add(exp2).print();
						//exp1.subtract(exp2).print();
						res.add(exp1.add(exp2));
						res.add(exp1.subtract(exp2));
						if (exp1.multiple(exp2).getOrder() < 3 && !exp2.equals(one)){
							//exp1.multiple(exp2).print();
							res.add(exp1.multiple(exp2));
						}
					}
				}
			}
			return res;
		}
	}
	
	public List<Expression> generateArith2(int num){
		List<Expression> vars = this.getVars();
		List<Expression> terms = this.getVars();
		int size = vars.size();
		for ( int i = 0; i < size; i++ ) {
			for ( int j = 0; j < i; j++) {
				terms.add(vars.get(i).multiple(vars.get(j)));
			}
		}
		Expression one = new Constant<Integer>(1);
		terms.add(one);
		List<Expression> res = new ArrayList<Expression>();
		if (num == 0) {
			return terms;
		} else {
			List<Expression> temp = this.generateArith2(num-1);
			for (Expression t : terms) {
				for (Expression e : temp){
					if (!t.equals(e)) {
						//t.add(e).print();
						res.add(t.add(e));
						res.add(t.subtract(e));
					}
				}
			}
			return res;
		}
	}
		
	public List<BoolExpression> generateBoolAux(int depth){
		List<Expression> vars = this.getVars();
		Expression one = new Constant<Integer>(1);
		Expression zero = new Constant<Integer>(0);
		if (depth == 0) {
			return null;
		} else {
			List<Expression> lst1 = this.generateArith(0);
			List<Expression> lst2 = this.generateArith(0);
			for(Expression e : vars){
				lst1.add(e.subtract(one));
				lst2.add(e.subtract(one));
			}
			lst1.add(zero);
			lst2.add(zero);
			List<BoolExpression> res = new ArrayList<BoolExpression>();
			int size1 = lst1.size();
			int size2 = lst2.size();
			for (int i = 0; i < size1; i++) {
				for (int j = 0; j < size2; j++) {
					Expression exp1 = lst1.get(i);
					Expression exp2 = lst2.get(j);
					if (!(exp1.toStringExp().equals(exp2.toStringExp()))) {
						res.add(exp1.binOp("==", exp2));
						//res.add(exp1.binOp("<", exp2));
						//res.add(exp1.binOp(">", exp2));
					}
				}
			}
			return res;
		}
	}
	
	public List<BoolExpression> generateBool(int depth){
		List<BoolExpression> lst1 = this.generateBoolAux(1);
		List<BoolExpression> lst2 = this.generateBoolAux(1);
		List<BoolExpression> res = lst1;
		if (depth == 0) {
			return res;
		} else {
			int size1 = lst1.size();
			int size2 = lst2.size();
			for (int i = 0; i < size1; i++) {
				BoolExpression exp1 = lst1.get(i);
				for (int j = 0; j < size2; j++) {
					res.add(exp1.unOp("!"));
					BoolExpression exp2 = lst2.get(j);
					if (!(exp1.toStringExp().equals(exp2.toStringExp()))) {
						res.add(exp1.binOp("&&", exp2));
						res.add(exp1.binOp("||", exp2));
						res.add(exp1.binOp("&&", exp2.unOp("!")));
						res.add(exp1.unOp("!").binOp("||", exp2));
					}
				}
			}
			return res;
		}
	}
}
