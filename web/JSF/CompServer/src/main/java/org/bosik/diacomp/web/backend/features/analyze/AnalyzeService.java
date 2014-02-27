package org.bosik.diacomp.web.backend.features.analyze;

import java.util.List;
import org.bosik.diacomp.web.backend.features.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.web.backend.features.analyze.entities.KoofList;

public interface AnalyzeService
{
	KoofList analyze(List<AnalyzeRec> recs);
}
