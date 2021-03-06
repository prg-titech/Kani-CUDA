package Expression;

import java.io.*;
import java.util.*;

public class Synthesizer {
	Environment env;
	List<String> vars;
	List<File> profiles;
	List<String> data;
	int limit = 0;
	

	public Synthesizer() {
		super();
		this.env = new Environment();
		this.vars = new ArrayList<String>();
		this.profiles = new ArrayList<File>();
		this.data = new ArrayList<String>();
	}
	
	public void inputVars(File file){
		try{
			if(file.exists()){
				FileReader fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String datum = br.readLine();
				this.vars = Arrays.asList(datum.split(" "));
				this.env = new Environment(this.vars);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void inputData(File file){
		this.data = new ArrayList<String>();
		try{
			if(file.exists()){
				FileReader fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String datum = "";
				
				limit = 0;
				
				// TODO Add datum when env.existsSmIdx() is true.
				while ((datum = br.readLine()) != null) {
					this.data.add(datum);
					this.env.setVal(vars, Arrays.asList(datum.split(" ")));
					if (this.env.exsitsSmIdx()) {
						this.limit++;
					}
				}
				fr.close();
				 
				this.limit = (int) (this.limit * 0.2);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void input(File profile){
		File vars = new File(profile, "vars");
		
		for(File file : profile.listFiles()){
			if (file.equals(vars)) {
				this.inputVars(file);
			} else {
				this.profiles.add(file);
			}
		}
	}


	public Expression synthesizeArith(int num){
		List<Expression> exps = env.generateArith2(num);
//		for(Expression ex : exps){
//			ex.print();
//		}
		Iterator<Expression> it = exps.iterator();
		int size = this.data.size();
		loop : while(it.hasNext()){
			Expression e = it.next();
			for(int i = 0; i < size; i++){
				env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
				if (env.exsitsSmIdx() && (e.eval(env) != env.getSmIdx())) {
					break;
				}
				if (i == size-1) {
					//System.out.printf("The number of variables: %d%n", this.vars.size()-2);
					//System.out.printf("The number of datum: %d%n", size);
					return e;
				}
			}
		}
		return new None();
	}
	
	public BoolExpression synthesizeBool(){
		List<BoolExpression> exps = env.generateBoolAux(1);
		Iterator<BoolExpression> it = exps.iterator();
		int size = this.data.size();
		while(it.hasNext()){
			BoolExpression e = it.next();
			boolean temp = false;
			for(int i = 0; i < size; i++){
				env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
				temp = env.exsitsSmIdx();
				if (e.eval(env) != temp) {
					break;
				}
				if (i == size-1) {
					//System.out.printf("The number of variables: %d%n", this.vars.size()-2);
					//System.out.printf("The number of datum: %d%n", size);
					//System.out.println("Synthesized expression:");
					//e.print();
					return e;
				}
			}
		}		
		exps = env.generateBool(1);
		it = exps.iterator();
		while(it.hasNext()){
			BoolExpression e = it.next();
			boolean temp = false;
			for(int i = 0; i < size; i++){
				env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
				temp = env.exsitsSmIdx();
				if (e.eval(env) != temp) {
					break;
				}
				if (i == size-1) {
					//System.out.printf("The number of variables: %d%n", this.vars.size()-2);
					//System.out.printf("The number of datum: %d%n", size);
					//System.out.println("Synthesized expression:");
					//e.print();
					return e;
				}
			}
		}
		return new BNone();
	}
	
	public Expression synthsizeIf() {
		List<String> thns = new ArrayList<String>();
		List<String> elss = new ArrayList<String>();
		Expression thn = new Constant<Integer>(0);
		Expression els = new Constant<Integer>(0);
		
		int size = this.data.size();
		int error = 0;
		
		loop : for (int k = 0; k < 3; k++){
			List<Expression> exps = env.generateArith2(k);
			Iterator<Expression> it = exps.iterator();

			while(it.hasNext()){
				Expression e = it.next();
				for(int i = 0; i < size; i++){
					env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
					if (env.exsitsSmIdx() && (e.eval(env) != env.getSmIdx())) {
						error++;
					}
					if (error > limit) {
						break;
					}
					if (i == size - 1) {
						thn = e;
						break loop;
					}
				}
				/*
				if (temp > score) {
					thn = e;
					score = temp;
				}
				*/
				error = 0;
			}
		}

		for(int i = 0; i < size; i++){
			env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
			if (env.exsitsSmIdx() && (thn.eval(env) == env.getSmIdx())) {
				thns.add(data.get(i));
			} else if (env.exsitsSmIdx()) {
				elss.add(data.get(i));
			}
		}
		//System.out.println(thns.size());
		//System.out.println(elss.size());

		int sizet = thns.size();
		int sizee = elss.size();
		
		loop : for (int h = 0; h < 2; h++) {
			List<Expression> exps2 = env.generateArith2(h);
			Iterator<Expression> it2 = exps2.iterator();
			
			while(it2.hasNext()){
				Expression e = it2.next();
				for(int i = 0; i < sizee; i++){
					env.setVal(vars, Arrays.asList(elss.get(i).split(" ")));
					if (e.eval(env) != env.getSmIdx()) {
						break;
					}
					if (i == sizee - 1) {
						els = e;
						break loop;
					}
				}
			}
		}
		
		
		List<BoolExpression> boolExps = env.generateBoolAux(1);
		
		Iterator<BoolExpression> it3 = boolExps.iterator();
		
		while(it3.hasNext()){
			BoolExpression e = it3.next();
			for (int i = 0; i < sizet; i++){
				env.setVal(vars, Arrays.asList(thns.get(i).split(" ")));
				if (!e.eval(env)) {
					break;
				}
				if (i == sizet - 1) {
					for (int j = 0; j < sizee; j++) {
						env.setVal(vars, Arrays.asList(elss.get(j).split(" ")));
						if (e.eval(env)) {
							break;
						}
						if (j == sizee - 1) {
							//System.out.printf("The number of variables: %d%n", this.vars.size()-2);
							//System.out.printf("The number of datum: %d%n", size);
							//System.out.println("Synthesized expression:");
							//new IfExpression(e, thn, els).print();
							return new IfExpression(e, thn, els);
						}
					}
				}
			}
		}
/*		
		boolExps = env.generateBool(1);
		
		it3 = boolExps.iterator();
		while(it3.hasNext()){
			BoolExpression e = it3.next();
			for (int i = 0; i < sizet; i++){
				env.setVal(vars, Arrays.asList(thns.get(i).split(" ")));
				if (!e.eval(env)) {
					break;
				}
				if (i == sizet - 1) {
					for (int j = 0; j < sizee; j++) {
						env.setVal(vars, Arrays.asList(elss.get(j).split(" ")));
						if (e.eval(env)) {
							break;
						}
						if (j == sizee - 1) {
							//System.out.printf("The number of variables: %d%n", this.vars.size()-2);
							//System.out.printf("The number of datum: %d%n", size);
							//System.out.println("Synthesized expression:");
							//new IfExpression(e, thn, els).print();
							return new IfExpression(e, thn, els);
						}
					}
				}
			}
		}*/
		return new None();
	}
	
	public String synthMemCopyExp(File profile){
		long start = System.currentTimeMillis();
		this.inputData(profile);
		
		List<BoolExpression> exps = env.generateBool(1);
		
		Iterator<BoolExpression> it = exps.iterator();
		int size = this.data.size();
		loop : while(it.hasNext()){
			BoolExpression e = it.next();
			boolean temp = false;
			for(int i = 0; i < size; i++){
				env.setVal(vars, Arrays.asList(data.get(i).split(" ")));
				temp = (env.getSmIdx() == 0);
				if (e.eval(env) != temp) {
					break;
				}
				if (i == size-1) {
					return e.toStringExp();
				}
			}
		}

		long end = System.currentTimeMillis();
		System.out.println("time: " + (end - start) + "ms");
		
		return "";	
	}
	
	public String synthMemExp(File profile){				
		long start, end;
		BoolExpression bnone = new BNone();
		Expression none = new None();
		BoolExpression bexp = bnone;
		Expression exp = none;
		this.inputData(profile);
		
		start = System.currentTimeMillis();
		bexp = this.synthesizeBool();
		
		if(!bexp.equals(bnone)){
			exp = this.synthesizeArith(3);
		}
		if(!bexp.equals(bnone)&&exp.equals(none)){
			exp = this.synthsizeIf();
		}
		if(exp.equals(none)){
			System.out.println("Not synthesized from " + profile.toString());
		} else {
			System.out.println("Expression synthesized from " + profile.toString() + ":");
			return bexp.toStringExp() + " ? sb[" + exp.toStringExp() + "] : ";
		}
		
		end = System.currentTimeMillis();
		System.out.println("time: " + (end - start) + "ms");
		return "";
	}	
	
	public void synthesizeFrom(File profiles){	
		long start = System.currentTimeMillis();
		
		File fr = new File("__ir.cu");
		ExpressionWriter writer = new ExpressionWriter();
		writer.input(fr);
		
		this.input(profiles);
		for(File file : this.profiles){
			String exp;
			if(file.getName().contains("forMemCopyExp")){
				exp = this.synthMemCopyExp(file);
				writer.assignMemCopyExp(exp, file.getName().substring("forMemCopyExp".length()));
			}else{
				exp = this.synthMemExp(file);
				writer.assignMemExp(exp, file.getName());
			}	
		}
		
		long end = System.currentTimeMillis();		
		
		System.out.println(writer.getCode());
		System.out.println("Synthesis time: " + (end - start) + "ms");
		
		writer.output();
	}
}
