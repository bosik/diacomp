package org.bosik.diacomp.core.persistence.serializers.ready;

import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Parser;
import org.bosik.diacomp.core.persistence.serializers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.serializers.utils.SerializerAdapter;

public class SerializerDiaryRecord implements Serializer<Versioned<DiaryRecord>>
{
	private Parser<DiaryRecord>					parser			= new ParserDiaryRecord();
	private Parser<Versioned<DiaryRecord>>		parserVersioned	= new ParserVersioned<DiaryRecord>(parser);
	private Serializer<Versioned<DiaryRecord>>	serializer		= new SerializerAdapter<Versioned<DiaryRecord>>(
																		parserVersioned);

	public Versioned<DiaryRecord> read(String s)
	{
		return serializer.read(s);
	}

	public List<Versioned<DiaryRecord>> readAll(String s)
	{
		return serializer.readAll(s);
	}

	public String write(Versioned<DiaryRecord> object)
	{
		return serializer.write(object);
	}

	public String writeAll(List<Versioned<DiaryRecord>> objects)
	{
		return serializer.writeAll(objects);
	}
}
