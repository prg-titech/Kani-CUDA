package Expression;

import java.io.*;
import java.util.*;

public class ExpressionWriter {
	String code;
	
	public ExpressionWriter() {
		super();
		this.code = "";
	}
	
	public String getCode(){
		return this.code;
	}
	
	public void input(File file){
		try{
			if(file.exists()){
				FileReader fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String datum = "";
				while ((datum = br.readLine()) != null) {
					this.code = this.code + "\n" + datum;
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}		
	}
	
	public void output(){
		File out = new File("synthesized.cu");
		try {
			out.createNewFile();
			FileWriter fw = new FileWriter(out);
			fw.write(this.code);
			fw.close();
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void assignMemExp(String exp, String id){			
		int bc = 0;
		int start = this.code.indexOf(id);
		int end = start + id.length();
		if(this.code.charAt(++end)=='('){
			bc++;
			end++;
			while(bc!=0){
				if (this.code.charAt(end)=='(') {
					bc++;
				} else if (this.code.charAt(end) == ')') {
					bc--;
				}
			}
		} else {
			while(this.code.charAt(++end)!=']'){
			}
			end+=2;
		}
		StringBuilder sb = new StringBuilder(this.code);
		sb.insert(start, "(");
		sb.insert(end, ")");
		sb.replace(start+1, start+id.length()+2, exp);
		this.code = sb.toString();					
	}
	
	public void assignMemCopyExp(String exp, String id){			
		int bc = 0;
		int start = id.length() + this.code.indexOf(id);
		int end = start;

		bc++;
		//System.out.println(this.code.charAt(end));
		while(bc!=0){
			++end;
			if (this.code.charAt(end) == '(') {
				bc++;
			} else if (this.code.charAt(end) == ')') {
				bc--;
			}
		}	

		StringBuilder sb = new StringBuilder(this.code);
		String[] memExps = sb.substring(start+1, end).toString().split(",");
		String globalMemExp = memExps[0].trim();
		String sharedMemExp = memExps[1].trim();
		String memCopyExp = "\nif" + exp + "{" + sharedMemExp + "=" + globalMemExp + ";}";

		sb.replace(this.code.indexOf(id), end+1, sharedMemExp);

		String insert = "__insert();";
		int insertPoint = this.code.indexOf(insert) + insert.length();
		sb.insert(insertPoint, memCopyExp);
		this.code = sb.toString();
	}
}
