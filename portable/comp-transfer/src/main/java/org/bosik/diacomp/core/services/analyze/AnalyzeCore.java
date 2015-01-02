package org.bosik.diacomp.core.services.analyze;

import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;

public interface AnalyzeCore
{
	/**
	 * Returns null if the list is empty
	 * 
	 * @param records
	 * @return
	 */
	KoofList analyze(List<Versioned<DiaryRecord>> records);
}
