package org.bosik.diacomp.core.persistence.serializers;

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;

public class SerializerDiaryRecord extends SerializerAdapter<Versioned<DiaryRecord>>
{
	public SerializerDiaryRecord()
	{
		super(new ParserVersioned<DiaryRecord>(new ParserDiaryRecord()));
	}
}
