package org.bosik.diacomp.web.backend.features.analyze.function;

import java.util.List;
import org.bosik.diacomp.web.backend.features.analyze.function.entities.AnalyzeRec;
import org.bosik.diacomp.web.backend.features.analyze.function.entities.KoofList;

public interface AnalyzeService
{
	KoofList analyze(List<AnalyzeRec> recs);
}
