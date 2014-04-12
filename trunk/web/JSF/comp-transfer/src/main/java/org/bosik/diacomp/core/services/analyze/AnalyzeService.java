package org.bosik.diacomp.core.services.analyze;

import java.util.List;
import org.bosik.diacomp.core.services.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;

public interface AnalyzeService
{
	/**
	 * Returns null if the list is empty
	 * 
	 * @param recs
	 * @return
	 */
	KoofList analyze(List<AnalyzeRec> recs);
}
