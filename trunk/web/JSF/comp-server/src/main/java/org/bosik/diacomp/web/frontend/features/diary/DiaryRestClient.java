package org.bosik.diacomp.web.frontend.features.diary;

@Deprecated
public class DiaryRestClient //extends AuthorizedRestClient implements DiaryService
{
	//	private static final long							serialVersionUID	= 1L;
	//
	//	private static Serializer<Versioned<DiaryRecord>>	serializer			= new SerializerDiaryRecord();
	//
	//	public DiaryRestClient(AuthService authService, String login, String pass, int apiVersion)
	//	{
	//		super(authService, login, pass, apiVersion);
	//	}
	//
	//	@Override
	//	public Versioned<DiaryRecord> findById(String id) throws CommonServiceException
	//	{
	//		try
	//		{
	//			WebResource resource = getResource(String.format("api/diary/guid/%s", id));
	//			String str = authGet(resource);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//
	//			return resp.getCode() != ResponseBuilder.CODE_NOTFOUND ? serializer.read(resp.getResponse()) : null;
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			handleUniformInterfaceException(e);
	//			return null; // previous method will throw exception anyway
	//		}
	//	}
	//
	//	@Override
	//	public List<Versioned<DiaryRecord>> findChanged(Date time) throws CommonServiceException
	//	{
	//		try
	//		{
	//			WebResource resource = getResource("api/diary/changes");
	//			resource = resource.queryParam("since", Utils.formatTimeUTC(time));
	//
	//			String str = authGet(resource);
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//
	//			return serializer.readAll(resp.getResponse());
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			handleUniformInterfaceException(e);
	//			return null; // previous method will throw exception anyway
	//		}
	//	}
	//
	//	@Override
	//	public List<Versioned<DiaryRecord>> findBetween(Date fromTime, Date toTime, boolean includeRemoved)
	//			throws CommonServiceException
	//	{
	//		try
	//		{
	//			WebResource resource = getResource("api/diary/period");
	//			resource = resource.queryParam("start_time", Utils.formatTimeUTC(fromTime));
	//			resource = resource.queryParam("end_time", Utils.formatTimeUTC(toTime));
	//			resource = resource.queryParam("show_rem", Utils.formatBooleanStr(includeRemoved));
	//
	//			String str = authGet(resource);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//
	//			return serializer.readAll(resp.getResponse());
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			handleUniformInterfaceException(e);
	//			return null; // previous method will throw exception anyway
	//		}
	//	}
	//
	//	@Override
	//	public void save(List<Versioned<DiaryRecord>> records) throws CommonServiceException
	//	{
	//		WebResource resource = getResource("api/diary/");
	//		try
	//		{
	//			Form form = new Form();
	//			form.add("items", serializer.writeAll(records));
	//			String str = authPut(resource, form);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			System.err.println(e.getResponse().getEntity(String.class));
	//			handleUniformInterfaceException(e);
	//		}
	//	}
	//
	//	@Override
	//	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	//	{
	//		Versioned<DiaryRecord> item = findById(id);
	//
	//		if (item == null)
	//		{
	//			throw new NotFoundException(id);
	//		}
	//		if (item.isDeleted())
	//		{
	//			throw new AlreadyDeletedException(id);
	//		}
	//
	//		item.setDeleted(true);
	//		save(Arrays.<Versioned<DiaryRecord>> asList(item));
	//	}
}
