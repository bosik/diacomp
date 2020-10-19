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
package org.bosik.diacomp.web.frontend.wicket.pages.stats;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.datetime.DateConverter;
import org.apache.wicket.datetime.markup.html.form.DateTextField;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.DownloadLink;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.file.Files;
import org.apache.wicket.util.time.Duration;
import org.bosik.diacomp.web.backend.features.report.ReportService;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.diacomp.web.frontend.wicket.components.UpdateOnBlurBehavior;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

public class StatsPage extends MasterPage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private UserInfoService userInfoService;

	@SpringBean
	private ReportService reportService;

	private Date   dateFrom;
	private Date   dateTo;
	private String reportFileName;

	public StatsPage(PageParameters parameters)
	{
		super(parameters);
		setPeriodMonth();
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final Form<Void> form = new Form<>("formDownload");
		add(form);

		final DateConverter converter = new DateConverter(false)
		{
			private static final String DATE_PATTERN = "yyyy-MM-dd";

			@Override
			public String getDatePattern(Locale locale)
			{
				return DATE_PATTERN;
			}

			@Override
			protected DateTimeFormatter getFormat(Locale locale)
			{
				return DateTimeFormat.forPattern(DATE_PATTERN);
			}
		};

		final DateTextField fieldDateFrom = new DateTextField("fieldDateFrom", new PropertyModel<>(this, "dateFrom"), converter);
		fieldDateFrom.add(new UpdateOnBlurBehavior());
		fieldDateFrom.setOutputMarkupId(true);
		form.add(fieldDateFrom);

		final DateTextField fieldDateTo = new DateTextField("fieldDateTo", new PropertyModel<>(this, "dateTo"), converter);
		fieldDateTo.add(new UpdateOnBlurBehavior());
		fieldDateTo.setOutputMarkupId(true);
		form.add(fieldDateTo);

		form.add(new AjaxFallbackLink<Void>("linkWeek")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setPeriodWeek();
				target.add(fieldDateFrom, fieldDateTo);
			}
		});

		form.add(new AjaxFallbackLink<Void>("linkMonth")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setPeriodMonth();
				target.add(fieldDateFrom, fieldDateTo);
			}
		});

		form.add(new AjaxFallbackLink<Void>("linkYear")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setPeriodYear();
				target.add(fieldDateFrom, fieldDateTo);
			}
		});

		final DownloadLink linkDownloadPdf = new DownloadLink("buttonDownloadPdf", new LoadableDetachableModel<File>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected File load()
			{
				if (dateFrom == null || dateTo == null)
				{
					error("Specify report dates");
					return null;
				}

				dateFrom = startOfDay(dateFrom);
				dateTo = endOfDay(dateTo);

				if (dateFrom.after(dateTo))
				{
					error("Invalid dates");
					return null;
				}

				if (datePeriod(dateFrom, dateTo) > 366)
				{
					error("Period must one year most");
					return null;
				}

				try
				{
					final ReportService.Report report = reportService.exportReportPdf(
							userInfoService.getCurrentUserId(),
							dateFrom, dateTo, getTimeZone());
					reportFileName = report.getFileName();
					return createTempFile(report.getContent());
				}
				catch (IOException e)
				{
					throw new RuntimeException(e);
				}
			}
		},
				new Model<String>()
				{
					@Override
					public String getObject()
					{
						return StringUtils.defaultIfBlank(reportFileName, "report.pdf");
					}
				}
		);
		linkDownloadPdf.setCacheDuration(Duration.NONE);
		linkDownloadPdf.setDeleteAfterDownload(true);
		form.add(linkDownloadPdf);

		final DownloadLink linkDownloadJson = new DownloadLink("buttonDownloadJson", new LoadableDetachableModel<File>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected File load()
			{
				if (dateFrom == null || dateTo == null)
				{
					error("Specify report dates");
					return null;
				}

				if (dateFrom.after(dateTo))
				{
					error("Invalid dates");
					return null;
				}

				try
				{
					final ReportService.Report report = reportService.exportReportJson(
							userInfoService.getCurrentUserId(),
							dateFrom, dateTo, getTimeZone());
					reportFileName = report.getFileName();
					return createTempFile(report.getContent());
				}
				catch (IOException e)
				{
					throw new RuntimeException(e);
				}
			}
		},
				new Model<String>()
				{
					@Override
					public String getObject()
					{
						return StringUtils.defaultIfBlank(reportFileName, "report.json");
					}
				}
		);
		linkDownloadJson.setCacheDuration(Duration.NONE);
		linkDownloadJson.setDeleteAfterDownload(true);
		form.add(linkDownloadJson);
	}

	private static long datePeriod(Date dateFrom, Date dateTo)
	{
		return (dateTo.getTime() - dateFrom.getTime()) / 1000 / 60 / 60 / 24;
	}

	private static File createTempFile(byte[] content) throws IOException
	{
		final File tempFile = File.createTempFile("report", ".tmp");
		try (final InputStream inputStream = new ByteArrayInputStream(content))
		{
			Files.writeTo(tempFile, inputStream);
		}
		return tempFile;
	}

	private void setPeriodYear()
	{
		setPeriod(Calendar.YEAR, -1);
	}

	private void setPeriodMonth()
	{
		setPeriod(Calendar.MONTH, -1);
	}

	private void setPeriodWeek()
	{
		setPeriod(Calendar.DATE, -7);
	}

	private void setPeriod(int field, int amount)
	{
		final Calendar c = getToday();
		endOfDay(c);
		dateTo = c.getTime();

		c.add(field, amount);
		startOfDay(c);
		dateFrom = c.getTime();
	}

	private Calendar getToday()
	{
		return Calendar.getInstance(getTimeZone());
	}

	private static void startOfDay(Calendar c)
	{
		c.set(Calendar.HOUR_OF_DAY, 0);
		c.set(Calendar.MINUTE, 0);
		c.set(Calendar.SECOND, 0);
		c.set(Calendar.MILLISECOND, 0);
	}

	private static void endOfDay(Calendar c)
	{
		c.set(Calendar.HOUR_OF_DAY, 23);
		c.set(Calendar.MINUTE, 59);
		c.set(Calendar.SECOND, 59);
		c.set(Calendar.MILLISECOND, 999);
	}

	private Date startOfDay(Date date)
	{
		final Calendar c = Calendar.getInstance(getTimeZone());
		c.setTime(date);
		startOfDay(c);
		return c.getTime();
	}

	private Date endOfDay(Date date)
	{
		final Calendar c = Calendar.getInstance(getTimeZone());
		c.setTime(date);
		endOfDay(c);
		return c.getTime();
	}
}
