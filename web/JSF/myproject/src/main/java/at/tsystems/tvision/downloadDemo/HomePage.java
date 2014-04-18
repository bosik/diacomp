package at.tsystems.tvision.downloadDemo;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.request.handler.resource.ResourceStreamRequestHandler;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.util.resource.AbstractResourceStreamWriter;
import org.bosik.diacomp.foodbase.FoodbasePage;

public class HomePage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	private static final String	FILENAME_ARCHIVE	= "log.zip";
	private static final String	FILENAME_LOGFILE	= "log.txt";

	public static byte[] zipBytes(String filename, byte[] input) throws IOException
	{
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ZipOutputStream zos = new ZipOutputStream(baos);
		ZipEntry entry = new ZipEntry(filename);
		entry.setSize(input.length);
		zos.putNextEntry(entry);
		zos.write(input);
		zos.closeEntry();
		zos.close();
		return baos.toByteArray();
	}

	private String getLogData()
	{
		StringBuilder sb = new StringBuilder("Well, this is the demo data with русский text. ");

		for (int i = 0; i < 22; i++)
		{
			sb.append(sb);
		}
		return sb.toString();
	}

	private void outputLog(OutputStream output) throws IOException
	{
		String data = getLogData();
		byte[] orgBytes = data.getBytes("UTF-8");
		byte[] zippedBytes = zipBytes(FILENAME_LOGFILE, orgBytes);
		output.write(zippedBytes);
	}

	public HomePage(final PageParameters parameters)
	{
		super(parameters);

		Link<Void> streamDownloadLink = new Link<Void>("downloadLog")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			public void onClick()
			{
				AbstractResourceStreamWriter rstream = new AbstractResourceStreamWriter()
				{
					private static final long	serialVersionUID	= 1L;

					@Override
					public void write(OutputStream output) throws IOException
					{
						outputLog(output);
					}
				};

				ResourceStreamRequestHandler handler = new ResourceStreamRequestHandler(rstream, FILENAME_ARCHIVE);
				getRequestCycle().scheduleRequestHandlerAfterCurrent(handler);
			}
		};

		add(streamDownloadLink);

		add(new BookmarkablePageLink<Void>("foodbase", FoodbasePage.class));
	}
}
