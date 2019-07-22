package Expression;

import java.io.*;
import java.util.*;

public class Synthesizer {
	List<String> vars;
	List<File> profiles;
	int[][] avail_arr;
	int[][] all_arr;
	int[] varIndex;
	int avail_line_count;
	int all_line_count;
	int max_error;
	int smidIndex;
	int[] allVarIndex;

	public Synthesizer() {
		super();
		this.vars = new ArrayList<String>();
		this.profiles = new ArrayList<File>();
	}
	
	public void inputVars(File file){
		try{
			if(file.exists()){
				FileReader fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String datum = br.readLine();
				this.vars = Arrays.asList(datum.split(" "));
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void addAllArr(List<String> lst) {
		for (int i = 0; i < vars.size(); i++) {
			if (lst.get(i).equals("N")) {
				all_arr[all_line_count][i] = -1;
				continue;
			}
			all_arr[all_line_count][i] = Integer.parseInt(lst.get(i));
		}
		all_line_count++;
	}
	
	public void addAvailArr(List<String> lst) {
		for (int i = 0; i < vars.size(); i++) {
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
		all_arr = new int[3000][this.vars.size()];
		avail_arr = new int[3000][this.vars.size()];
	}


	public String synthesizeArith(int num){

		LinearArithExpression exp = new LinearArithExpression(new int[] {1, 2},
				varIndex, avail_arr, smidIndex, avail_line_count, vars);
		String result = exp.generate(num);
		System.out.println(result);
		return result;
	}

	public String synthesizeBool(){

		int[] allVar = new int[vars.size() - smidIndex - 1];
		for (int i = smidIndex + 1; i < vars.size(); i++) {
			allVar[i - smidIndex - 1] = i;
		}
		LinearLogicExpression exp = new LinearLogicExpression(new int[] {1}, allVar,
				all_arr, smidIndex, all_line_count, vars);
		String result = exp.generate();
		System.out.println(result);
		return result;
	}
	
	public String synthesizeIf() {
		LinearArithExpression exp = new LinearArithExpression(new int[] {1, 2},
				varIndex, avail_arr, smidIndex, avail_line_count, vars);
		String result = exp.generate_partial(3, max_error);
		System.out.println(result);
		return result;
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
			//TODO arith_exp = synthesizeIf
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
	
	public void synthesizeFrom(File profiles){	
		long start = System.currentTimeMillis();
		
		File fr = new File("__ir.cu");
		ExpressionWriter writer = new ExpressionWriter();
		writer.input(fr);
		
		this.input(profiles);
		for(File file : this.profiles){
			String exp;
			PearsonCorrelation pc = new PearsonCorrelation(vars, avail_arr, avail_line_count);
			pc.process();
			varIndex = pc.getVarIndex();
			smidIndex = pc.getSmid();
			if (file.getName().contains("forMemCopyExp")) {
				exp = this.synthMemCopyExp(file);
				writer.assignMemCopyExp(exp, file.getName().substring("forMemCopyExp".length()));
			} else {
				exp = this.synthMemExp(file);
				writer.assignMemExp(exp, file.getName());
			}	
		}
		
		long end = System.currentTimeMillis();		
		
		//System.out.println(writer.getCode());
		System.out.println("Synthesis time: " + (end - start) + "ms");
		
		writer.output();
	}
}
