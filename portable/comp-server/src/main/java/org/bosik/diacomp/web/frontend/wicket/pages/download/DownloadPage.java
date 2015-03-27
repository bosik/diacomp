package org.bosik.diacomp.web.frontend.wicket.pages.download;

import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;

public class DownloadPage extends MasterPage
{
	private static final long	serialVersionUID	= 1L;

	public DownloadPage(PageParameters parameters)
	{
		super(parameters);
		add(new ExternalLink("linkAppWindows", "api/windows/file/compensation_setup.exe"));
	}
}
