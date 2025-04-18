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

import lombok.RequiredArgsConstructor;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.report.data.BsUnit;
import org.bosik.diacomp.web.backend.features.user.auth.UserRest;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.ws.rs.core.HttpHeaders;
import java.io.IOException;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

@RestController
@RequiredArgsConstructor
@RequestMapping("/report")
public class ReportRest extends UserRest
{
	private static final int MAX_DATETIME_SIZE = Utils.FORMAT_DATE_TIME.length();
	private static final int MAX_TIMEZONE_SIZE = "GMT+00:00".length();
	private static final int MAX_LOCALE_SIZE   = "en".length();

	private final ReportService reportService;

	@GetMapping(path = "/pdf", produces = "application/pdf")
	public ResponseEntity<byte[]> exportPdfReport(
			@RequestParam("from") String parFromDate,
			@RequestParam("to") String parToDate,
			@RequestParam(name = "timeZone", defaultValue = "UTC") String parTimeZone,
			@RequestParam(name = "locale", defaultValue = "en") String parLocale,
			@RequestParam(name = "bsUnit", defaultValue = "mmoll") String parBsUnit
	) throws IOException
	{
		final int userId = getUserId();
		final Date fromDate = safeParseTimeUTC(parFromDate);
		final Date toDate = safeParseTimeUTC(parToDate);
		final TimeZone timeZone = safeParseTimeZone(parTimeZone);
		final Locale locale = safeParseLocale(parLocale);
		final BsUnit bsUnit = safeParseBsUnit(parBsUnit);

		final ReportService.Report report = reportService.exportReportPdf(userId, fromDate, toDate, bsUnit, timeZone, locale);

		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + report.getFileName() + "\"")
				.body(report.getContent());
	}

	private static Date safeParseTimeUTC(String s)
	{
		Utils.checkSize(s, MAX_DATETIME_SIZE);
		return Utils.parseTimeUTC(s);
	}

	private static TimeZone safeParseTimeZone(String s)
	{
		Utils.checkSize(s, MAX_TIMEZONE_SIZE);
		return TimeZone.getTimeZone(s);
	}

	private static Locale safeParseLocale(String s)
	{
		Utils.checkSize(s, MAX_LOCALE_SIZE);
		try
		{
			return new Locale(s);
		}
		catch (Exception e)
		{
			return Locale.ENGLISH;
		}
	}

	private static BsUnit safeParseBsUnit(String s)
	{
		Utils.checkSize(s, 5);

		try
		{
			switch (s.toLowerCase())
			{
				case "mmoll":
					return BsUnit.MMOL_L;
				case "mgdl":
					return BsUnit.MG_DL;
			}
		}
		catch (Exception e)
		{
			// ignore
		}

		return BsUnit.MMOL_L;
	}
}
