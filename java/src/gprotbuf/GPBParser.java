package gprotbuf;

import java.util.Map;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.Keyword;
import gprotbuf.exception.ParserException;

public class GPBParser {
	private static final String GPROTBUF_CORE = "gprotbuf.core";
	private static final String CLOJURE_CORE = "clojure.core";
	private static final String INSTAPARSE_CORE= "instaparse.core";
	private final IFn require;
	private final IFn isSyntax;
	private final IFn parseBlock;
	private final IFn meta;
	private final IFn isFailure;
	private final IFn transform;
	private final IFn parse;
	private final IFn isPrimitiveType;
	private final static GPBParser self = new GPBParser();
	public static final GPBParser instance() {
		return self;
	}
	private GPBParser() {
		require = Clojure.var(CLOJURE_CORE, "require");
		meta = Clojure.var(CLOJURE_CORE, "meta");
		require.invoke(Clojure.read(GPROTBUF_CORE));
		parseBlock = Clojure.var(GPROTBUF_CORE, "parse-block");
		parse = Clojure.var(GPROTBUF_CORE, "parse");
		transform = Clojure.var(GPROTBUF_CORE, "ast->clj");
		isFailure = Clojure.var(INSTAPARSE_CORE, "failure?");
		isSyntax = Clojure.var(GPROTBUF_CORE, "syntax?");
		isPrimitiveType = Clojure.var(GPROTBUF_CORE, "primitive-type?");
	}
	
	/**
	 * Parse a protobuf text starts with '{' and ends with '};'
	 * @param text
	 * @return
	 */
	public final Object parseBlock(final String text) {
		return parseBlock.invoke(text);
	}
	
	/**
	 * Parse a protobuf version 3 text. Check for failure with isFailure
	 * @param text
	 * @return
	 */
	public final Object parse(final String text) {
		return parse.invoke(text);
	}

	public final boolean isPrimitiveType(final String type) {
		return (boolean) isPrimitiveType.invoke(type);
	}
	
	
	/**
	 * Transform and clean the ast to produce a new ast that is a little easier to navigate.
	 * Also perform some validation like ensure that tags don't overlap etc.
	 * @throws ParserException if something does not validate. 
	 * @param ast the cleaned ast.
	 * @return
	 */
	public final Object transform(final Object ast) {
		return transform.invoke(ast);
	}
	
	public boolean isFailure(final Object ast) {
		return (boolean) isFailure.invoke(ast);
	}
	
	public static class ColLine {

		private final Map<Keyword, Long> ast;

		public ColLine(final Map<Keyword, Long> ast) {
			this.ast = ast;
		}
		
		public long line() { return ast.get(Keyword.intern(null, "line")); }
		public long column() { return ast.get(Keyword.intern(null, "column")); }
		@Override
		public String toString() {
			return String.format("column=%s, line=%s", column(), line());
		}
		
	}
	/**
	 * Get the column and line for the failure
	 * @param ast
	 * @return
	 */
	public ColLine getFailureColLine(final Object ast) {
		return new ColLine((Map<Keyword, Long>) ast);
	}
	
	public static class MetaInfo {

		private final Map<Keyword, Long> node;

		public MetaInfo(final Map<Keyword, Long> node) {
			this.node = node;
		}
		public long indexOf(final String name) { return node.get(Keyword.intern("instaparse.gll", name)); }
		
		public long startIndex() { return indexOf("start-index"); }
		public long endIndex() { return indexOf("end-index"); }
		public long startLine() { return indexOf("start-line"); }
		public long startColumn() { return indexOf("start-column"); }
		public long endLine() { return indexOf("end-line"); }
		public long endColumn() { return indexOf("end-column"); }
		@Override
		public String toString() {
			return node.toString();
		}
	
	}
	
	/**
	 * Get the meta info such as column and line of the ast.
	 * @param node
	 * @return
	 */
	public MetaInfo metaInfoOf(final Object node) {
		return new MetaInfo((Map<Keyword, Long>) meta.invoke(node));
	}

	
	/**
	 * Checks if it's a proto3 text block starting with '{' and ending with '};' 
	 * @see parseBlock
	 * @param text
	 * @return
	 */
	public final boolean isSyntax(final String text) {
		return (boolean) isSyntax.invoke(text);
	}
	
	public final boolean isKeyword(final Object kwObj, final String key) {
		return Keyword.intern(null, key).equals(kwObj);
	}
	
	public static void main(final String[] args) {
		final GPBParser p = new GPBParser();
		final Object ast = p.parseBlock("{ syntaxa=\"proto3\"; };");
		if (p.isFailure(ast)) {
			System.out.println(p.getFailureColLine(ast));
		} else {
			System.out.println(p.metaInfoOf(ast).startIndex());
		}
	}
	
}
