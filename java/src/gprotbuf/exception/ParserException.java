package gprotbuf.exception;

import java.util.HashMap;
import java.util.Map;

public class ParserException extends RuntimeException {
	public final long line;
	public final long column;

	public ParserException(final String msg, final long line, final long column) {
		super(msg);
		this.line = line;
		this.column = column;
	}

	@Override
	public String toString() {
		final Map<String, Object> m = new HashMap<>();
		m.put("msg", getMessage());
		m.put("line", line);
		m.put("column", column);
		return m.toString();
	}
	
}
