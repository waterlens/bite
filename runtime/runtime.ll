; ModuleID = 'runtime.c'
source_filename = "runtime.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.object = type { i32, i8, i8, i16 }
%struct.string = type { %struct.object, i32, [0 x i8] }
%struct.array = type { %struct.object, i8*, i32, i32 }
%struct.closure = type { %struct.object, i8*, i8* }
%struct.tagged = type { %struct.object, i32, i32, [0 x i8*] }
%struct.tuple = type { %struct.object, i32, i32, [0 x i8*] }

@.str = private unnamed_addr constant [37 x i8] c"the number of tuple fields exceeds 8\00", align 1
@.str.1 = private unnamed_addr constant [20 x i8] c"extraction overflow\00", align 1
@.str.2 = private unnamed_addr constant [36 x i8] c"the number of ctor fields exceeds 8\00", align 1
@.str.3 = private unnamed_addr constant [17 x i8] c"inconsistent tag\00", align 1

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone sspstrong uwtable willreturn
define dso_local void @_dummy(%struct.object* nocapture noundef %0, %struct.string* nocapture noundef %1, %struct.array* nocapture noundef %2, %struct.closure* nocapture noundef %3, %struct.tagged* nocapture noundef %4, %struct.tuple* nocapture noundef %5) local_unnamed_addr #0 {
  ret void
}

; Function Attrs: noreturn nounwind sspstrong uwtable
define dso_local void @runtime_panic(i8* nocapture noundef readonly %0) local_unnamed_addr #1 {
  %2 = tail call i32 @puts(i8* noundef nonnull dereferenceable(1) %0)
  tail call void @exit(i32 noundef 1) #7
  unreachable
}

; Function Attrs: nofree nounwind
declare noundef i32 @puts(i8* nocapture noundef readonly) local_unnamed_addr #2

; Function Attrs: noreturn nounwind
declare void @exit(i32 noundef) local_unnamed_addr #3

; Function Attrs: nounwind sspstrong uwtable
define dso_local noalias %struct.tuple* @alloc_tuple(i64 noundef %0) local_unnamed_addr #4 {
  %2 = icmp ugt i64 %0, 8
  br i1 %2, label %3, label %5

3:                                                ; preds = %1
  %4 = tail call i32 @puts(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([37 x i8], [37 x i8]* @.str, i64 0, i64 0)) #8
  tail call void @exit(i32 noundef 1) #7
  unreachable

5:                                                ; preds = %1
  %6 = shl nuw nsw i64 %0, 3
  %7 = add nuw nsw i64 %6, 16
  %8 = tail call noalias i8* @GC_malloc(i64 noundef %7) #9
  %9 = bitcast i8* %8 to %struct.tuple*
  %10 = getelementptr inbounds i8, i8* %8, i64 4
  store i8 2, i8* %10, align 1, !tbaa !5
  %11 = trunc i64 %0 to i32
  %12 = getelementptr inbounds i8, i8* %8, i64 8
  %13 = bitcast i8* %12 to i32*
  store i32 %11, i32* %13, align 1, !tbaa !11
  ret %struct.tuple* %9
}

; Function Attrs: allocsize(0)
declare noalias i8* @GC_malloc(i64 noundef) local_unnamed_addr #5

; Function Attrs: mustprogress nofree norecurse nosync nounwind readonly sspstrong uwtable willreturn
define dso_local i64 @get_tuple_size(%struct.tuple* nocapture noundef readonly %0) local_unnamed_addr #6 {
  %2 = getelementptr inbounds %struct.tuple, %struct.tuple* %0, i64 0, i32 1
  %3 = load i32, i32* %2, align 1, !tbaa !11
  %4 = zext i32 %3 to i64
  ret i64 %4
}

; Function Attrs: nounwind sspstrong uwtable
define dso_local i8* @extract_tuple_field(%struct.tuple* nocapture noundef readonly %0, i64 noundef %1) local_unnamed_addr #4 {
  %3 = getelementptr inbounds %struct.tuple, %struct.tuple* %0, i64 0, i32 1
  %4 = load i32, i32* %3, align 1, !tbaa !11
  %5 = zext i32 %4 to i64
  %6 = icmp ugt i64 %5, %1
  br i1 %6, label %9, label %7

7:                                                ; preds = %2
  %8 = tail call i32 @puts(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([20 x i8], [20 x i8]* @.str.1, i64 0, i64 0)) #8
  tail call void @exit(i32 noundef 1) #7
  unreachable

9:                                                ; preds = %2
  %10 = getelementptr inbounds %struct.tuple, %struct.tuple* %0, i64 0, i32 3, i64 %1
  %11 = load i8*, i8** %10, align 1, !tbaa !12
  ret i8* %11
}

; Function Attrs: nounwind sspstrong uwtable
define dso_local noalias %struct.tagged* @alloc_tagged(i64 noundef %0, i64 noundef %1) local_unnamed_addr #4 {
  %3 = icmp ugt i64 %1, 8
  br i1 %3, label %4, label %6

4:                                                ; preds = %2
  %5 = tail call i32 @puts(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([36 x i8], [36 x i8]* @.str.2, i64 0, i64 0)) #8
  tail call void @exit(i32 noundef 1) #7
  unreachable

6:                                                ; preds = %2
  %7 = shl nuw nsw i64 %1, 3
  %8 = add nuw nsw i64 %7, 16
  %9 = tail call noalias i8* @GC_malloc(i64 noundef %8) #9
  %10 = bitcast i8* %9 to %struct.tagged*
  %11 = getelementptr inbounds i8, i8* %9, i64 4
  store i8 2, i8* %11, align 1, !tbaa !5
  %12 = trunc i64 %0 to i32
  %13 = getelementptr inbounds i8, i8* %9, i64 8
  %14 = bitcast i8* %13 to i32*
  store i32 %12, i32* %14, align 1, !tbaa !11
  %15 = trunc i64 %1 to i32
  %16 = getelementptr inbounds i8, i8* %9, i64 12
  %17 = bitcast i8* %16 to i32*
  store i32 %15, i32* %17, align 1, !tbaa !11
  ret %struct.tagged* %10
}

; Function Attrs: nounwind sspstrong uwtable
define dso_local i8* @extract_tagged_field(%struct.tagged* nocapture noundef readonly %0, i64 noundef %1, i64 noundef %2) local_unnamed_addr #4 {
  %4 = getelementptr inbounds %struct.tagged, %struct.tagged* %0, i64 0, i32 1
  %5 = load i32, i32* %4, align 1, !tbaa !11
  %6 = zext i32 %5 to i64
  %7 = icmp eq i64 %6, %1
  br i1 %7, label %10, label %8

8:                                                ; preds = %3
  %9 = tail call i32 @puts(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([17 x i8], [17 x i8]* @.str.3, i64 0, i64 0)) #8
  tail call void @exit(i32 noundef 1) #7
  unreachable

10:                                               ; preds = %3
  %11 = getelementptr inbounds %struct.tagged, %struct.tagged* %0, i64 0, i32 2
  %12 = load i32, i32* %11, align 1, !tbaa !11
  %13 = zext i32 %12 to i64
  %14 = icmp ugt i64 %13, %2
  br i1 %14, label %17, label %15

15:                                               ; preds = %10
  %16 = tail call i32 @puts(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([20 x i8], [20 x i8]* @.str.1, i64 0, i64 0)) #8
  tail call void @exit(i32 noundef 1) #7
  unreachable

17:                                               ; preds = %10
  %18 = getelementptr inbounds %struct.tagged, %struct.tagged* %0, i64 0, i32 3, i64 %2
  %19 = load i8*, i8** %18, align 1, !tbaa !12
  ret i8* %19
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone sspstrong uwtable willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { noreturn nounwind sspstrong uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nofree nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { noreturn nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { nounwind sspstrong uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { allocsize(0) "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #6 = { mustprogress nofree norecurse nosync nounwind readonly sspstrong uwtable willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #7 = { noreturn nounwind }
attributes #8 = { nounwind }
attributes #9 = { nounwind allocsize(0) }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{!"clang version 14.0.6"}
!5 = !{!6, !8, i64 4}
!6 = !{!"object", !7, i64 0, !8, i64 4, !8, i64 5, !10, i64 6}
!7 = !{!"int", !8, i64 0}
!8 = !{!"omnipotent char", !9, i64 0}
!9 = !{!"Simple C/C++ TBAA"}
!10 = !{!"short", !8, i64 0}
!11 = !{!7, !7, i64 0}
!12 = !{!13, !13, i64 0}
!13 = !{!"any pointer", !8, i64 0}
