package gprotbuf;

import java.util.List;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class GPBParser {
	private static final String CLOJURE_CORE = "clojure.core";
	private final IFn require;
	private final IFn parse;
	public GPBParser() {
		require = Clojure.var(CLOJURE_CORE, "require");
		require.invoke(Clojure.read("gprotbuf.core"));
		parse = Clojure.var("gprotbuf.core", "parse");
	}

	public final List parse(final String text) {
		final Object res = parse.invoke(text);
		if (res instanceof List) {
			return (List)res;
		}
		throw new IllegalStateException();
	}
	
	public static void main(String[] args) {
		System.out.println(new GPBParser().parse("syntax = \"proto3\";"));
	}
}
