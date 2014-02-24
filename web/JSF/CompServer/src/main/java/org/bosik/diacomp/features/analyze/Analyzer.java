package org.bosik.diacomp.features.analyze;

import java.util.List;

import org.bosik.diacomp.features.analyze.entities.AnalyzeRec;
import org.bosik.diacomp.features.analyze.entities.KoofList;

public interface Analyzer
{
	KoofList analyze(List<AnalyzeRec> recs);
}
