package org.bosik.diacomp.core.services.analyze;

import java.util.List;
import org.bosik.diacomp.core.services.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;

public interface AnalyzeService
{
	KoofList analyze(List<AnalyzeRec> recs);
}
