package Expression;

import java.io.*;
import java.util.*;

public class Synthesizer {
	List<String> var_names;	// list of all variable names
	List<File> profiles;	// list of all profile names
	int[][] avail_arr;		// all profiles with available shared memory id
	int[][] all_arr;		// all profiles
	int[] rel_var_index;	// includes constant variables and variables passed correlation test
	int[] all_var_index;	// does not include variables before shared memory id
	int avail_line_count;	// number of lines in avail_arr
	int all_line_count;		// number of lines in all_arr
	int max_error;			// max error for generating conditional expressions
	int smidIndex;			// index of shared memory id in variable list, skip the variables before it

	public Synthesizer() {
		super();
		this.var_names = new ArrayList<String>();
		this.profiles = new ArrayList<File>();
	}
	
	public void inputVars(File file){
		try{
			if(file.exists()){
				FileReader fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String datum = br.readLine();
				this.var_names = Arrays.asList(datum.split(" "));
				br.close();
				fr.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void addAllArr(List<String> lst) {
		for (int i = 0; i < var_names.size(); i++) {
			if (lst.get(i).equals("N")) {
				all_arr[all_line_count][i] = -1;
				continue;
			}
			all_arr[all_line_count][i] = Integer.parseInt(lst.get(i));
		}
		all_line_count++;
	}
	
	public void addAvailArr(List<String> lst) {
		for (int i = 0; i < var_names.size(); i++) {
			avail_arr[avail_line_count][i] = Integer.parseInt(lst.get(i));
		}
		avail_line_count++;
	}

	public void inputData(File file){
		try {
			if(file.exists()) {
				FileReader fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String line;
				
				max_error = 0;
				avail_line_count = 0;
				all_line_count = 0;
				
				while ((line = br.readLine()) != null) {
					List<String> lst = Arrays.asList(line.split(" "));
					if (!lst.contains("N")) {
						addAvailArr(lst);
						max_error++;
					}
					addAllArr(lst);
				}
				max_error = (int) (max_error * 0.2);
				br.close();
				fr.close();
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
		//TODO hardcoded this size
		all_arr = new int[3000][this.var_names.size()];
		avail_arr = new int[3000][this.var_names.size()];
	}


	public String synthesizeArith(int num){
		LinearArithExpression exp = new LinearArithExpression(new int[] {1, 2},
				rel_var_index, avail_arr, smidIndex, avail_line_count, var_names);
		String result = exp.generate(num);
		return result;
	}

	public String synthesizeBool(){
		LinearLogicExpression exp = new LinearLogicExpression(new int[] {1}, all_var_index,
				all_arr, smidIndex, all_line_count, var_names);
		String result = exp.generate();
		return result;
	}
	
	public String synthesizeIf() {
		// find first arith expression
		LinearArithExpression exp = new LinearArithExpression(new int[] {1, 2},
				rel_var_index, avail_arr, smidIndex, avail_line_count, var_names);
		String firstArith = exp.generate_partial(3, max_error);
		if (firstArith.equals("f")) { return firstArith; }
		
		// find conditional statement
		int[][] conditionalProfile = exp.getPartial();
		LinearLogicExpression lexp = new LinearLogicExpression(new int[] {1}, all_var_index,
				conditionalProfile, smidIndex, avail_line_count, var_names);
		String condition = lexp.generate();
		if (condition.equals("f")) { return condition; }
		
		// find second arith expression
		LinearArithExpression sexp = new LinearArithExpression(new int[] {1, 2},
				rel_var_index, conditionalProfile, smidIndex, avail_line_count, var_names);
		String secondArith = sexp.generate(3);
		if (secondArith.equals("f")) { return secondArith; }
		return condition + " ?" + firstArith + ":"  + secondArith;
	}
	
	public String synthMemCopyExp(File profile){
		/*
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
		*/
		
		return "";	
	}
	
	public String synthMemExp(File profile){

		this.inputData(profile);

		String logic_exp = synthesizeBool();
		if (logic_exp.equals("f")) {
			System.out.println("Not synthesized from " + profile.toString());
			return "";
		}
		String arith_exp = synthesizeArith(3);
		if (arith_exp.equals("f")) {
			arith_exp = synthesizeIf();
		}
		if (arith_exp.equals("f")) {
			System.out.println("Not synthesized from " + profile.toString());
			return "";
		} else {
			System.out.println("Expression synthesized from " + profile.toString() + ":");
			return logic_exp + " ? sb[" + arith_exp + "] : ";
		}
	}	
	
	// Interface
	public void synthesizeFrom(File profiles){	
		long start = System.currentTimeMillis();
		
		File fr = new File("__ir.cu");
		ExpressionWriter writer = new ExpressionWriter();
		writer.input(fr);
		
		this.input(profiles);
		for(File file : this.profiles){
			String exp;
			PearsonCorrelation pc = new PearsonCorrelation(var_names, avail_arr, avail_line_count);
			pc.process();
			rel_var_index = pc.getVarIndex();
			smidIndex = pc.getSmid();
			all_var_index = new int[var_names.size() - smidIndex - 1];
			for (int i = smidIndex + 1; i < var_names.size(); i++) {
				all_var_index[i - smidIndex - 1] = i;
			}
			if (file.getName().contains("forMemCopyExp")) {
				exp = this.synthMemCopyExp(file);
				writer.assignMemCopyExp(exp, file.getName().substring("forMemCopyExp".length()));
			} else {
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
