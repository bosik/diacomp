/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.report;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.layout.Document;
import lombok.Value;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.web.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.web.backend.features.report.data.BsUnit;
import org.bosik.diacomp.web.backend.features.report.data.Statistics;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

@Service
public class ReportService
{
	// FIXME
	private static final double minBs = 3.7;
	private static final double maxBs = 7.8;

	@Autowired
	private DiaryLocalService diaryService;

	private Statistics getStatistics(int userId, Date fromDate, Date toDate, BsUnit bsUnit, TimeZone timeZone, Locale locale)
	{
		final List<Versioned<DiaryRecord>> records = diaryService.findPeriod(userId, fromDate, toDate, false);
		return new Statistics(records, fromDate, toDate, minBs, maxBs, bsUnit, timeZone, locale);
	}

	public Report exportReportJson(int userId, Date fromDate, Date toDate, BsUnit bsUnit, TimeZone timeZone, Locale locale)
			throws IOException
	{
		final Statistics statistics = getStatistics(userId, fromDate, toDate, bsUnit, timeZone, locale);

		return new Report(
				getFileName(statistics) + ".json",
				new ObjectMapper().writeValueAsString(statistics).getBytes()
		);
	}

	public Report exportReportPdf(int userId, Date fromDate, Date toDate, BsUnit bsUnit, TimeZone timeZone, Locale locale)
			throws IOException
	{
		final Statistics statistics = getStatistics(userId, fromDate, toDate, bsUnit, timeZone, locale);

		final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		try (final Document doc = new Document(new PdfDocument(new PdfWriter(outputStream))))
		{
			ReportBuilder.exportData(doc, statistics);
		}

		return new Report(
				getFileName(statistics) + ".pdf",
				outputStream.toByteArray()
		);
	}

	private static String getFileName(Statistics statistics)
	{
		return "diacomp_report_" + statistics.getDateStart() + "_" + statistics.getDateEnd();
	}

	@Value
	public static class Report
	{
		private String fileName;
		private byte[] content;
	}
}
