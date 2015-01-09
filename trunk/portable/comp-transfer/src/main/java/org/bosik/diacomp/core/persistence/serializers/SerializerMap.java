package org.bosik.diacomp.core.persistence.serializers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserMapEntry;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;

public class SerializerMap implements Serializer<Map<String, String>>
{
	private Parser<Entry<String, String>>		parserEntry		= new ParserMapEntry();
	private Serializer<Entry<String, String>>	serializerEntry	= new SerializerAdapter<Entry<String, String>>(
																		parserEntry);

	@Override
	public Map<String, String> read(String s)
	{
		Map<String, String> map = new HashMap<String, String>();

		List<Entry<String, String>> pairs = serializerEntry.readAll(s);
		for (Entry<String, String> pair : pairs)
		{
			map.put(pair.getKey(), pair.getValue());
		}

		return map;
	}

	@Override
	public String write(Map<String, String> object)
	{
		List<Entry<String, String>> pairs = new ArrayList<Entry<String, String>>();
		pairs.addAll(object.entrySet());
		return serializerEntry.writeAll(pairs);
	}

	@Override
	public List<Map<String, String>> readAll(String s)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public String writeAll(List<Map<String, String>> objects)
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
